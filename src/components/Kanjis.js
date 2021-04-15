import React, { useState, useEffect } from 'react';
import { DataGrid } from '@material-ui/data-grid';

import styled from 'styled-components';

import Snackbar from '@material-ui/core/Snackbar';
import IconButton from '@material-ui/core/IconButton';
import CloseIcon from '@material-ui/icons/Close';
import Button from '@material-ui/core/Button';
import { createMuiTheme, ThemeProvider as MuiThemeProvider } from '@material-ui/core/styles';

import { joyoList, jinmeiyoList } from './joyojinmeiyo';

const theme = createMuiTheme({
	palette: {
		primary: {
			main: '#2B99CF',
		},
	},
});

export default function Kanjis({ data }) {
	const columns = [
		{
			field: 'id',
			headerName: 'Kanji',
			width: 120,
			renderCell: (params) => (
				<>
					{params.value}
					<Button
						variant="outlined"
						color="primary"
						size="small"
						style={{ margin: ' 0px 10px' }}
						onClick={() => handleCopy(params.value)}
					>
						copy
					</Button>
				</>
			),
		},

		{ field: 'type', headerName: 'Type', width: 90 },
		{ field: 'taughtIn', headerName: 'Taught in', width: 150 },
		{ field: 'jlptLevel', headerName: 'JLPT', width: 90 },
		{ field: 'newspaperFrequencyRank', headerName: 'Rank', type: 'number', width: 90 },
		{ field: 'strokeCount', headerName: 'Strokes', type: 'number', width: 110 },
		{ field: 'meaning', headerName: 'Meaning', width: 200 },
		{ field: 'kunyomi', headerName: 'Kunyomi', width: 150 },
		{ field: 'onyomi', headerName: 'Onyomi', width: 150 },
		{
			field: 'radical',
			headerName: 'Radical',
			width: 120,
			renderCell: (params) => (
				<>
					{params.value}
					<Button
						variant="outlined"
						color="primary"
						size="small"
						style={{ margin: ' 0px 10px' }}
						onClick={() => handleCopy(params.value)}
					>
						copy
					</Button>
				</>
			),
		},
	];

	useEffect(() => {
		if (data) {
			let rowsCopy = [];
			for (const [kanji, kanjidata] of Object.entries(data)) {
				if (kanji.length <= 4) {
					let type = '';
					if (jinmeiyoList?.includes(kanji)) {
						type = 'jinmeiyō';
					}
					if (joyoList?.includes(kanji)) {
						type = 'jōyō';
					}

					rowsCopy.push({
						id: kanji,
						type: type,
						taughtIn: kanjidata?.jishoData?.taughtIn,
						jlptLevel: kanjidata?.jishoData?.jlptLevel,
						newspaperFrequencyRank: kanjidata?.jishoData?.taunewspaperFrequencyRankghtIn,
						strokeCount: kanjidata?.jishoData?.strokeCount,
						meaning: kanjidata?.jishoData?.meaning,
						kunyomi: kanjidata?.jishoData?.kunyomi,
						onyomi: kanjidata?.jishoData?.onyomi,
						radical: kanjidata?.jishoData?.radical?.symbol,
					});
				}
			}
			setRows(rowsCopy);
		}
	}, [data]);

	const [rows, setRows] = useState([]);

	const handleCopy = (clickedKanji) => {
		setClipboardKanji(clickedKanji);
		const dummy = document.createElement('textarea');
		dummy.value = clickedKanji;
		document.body.appendChild(dummy);
		dummy.select();
		document.execCommand('copy');
		document.body.removeChild(dummy);
		setSnackbarOpen(true);
	};

	const [snackbarOpen, setSnackbarOpen] = useState(false);

	const [clipboardKanji, setClipboardKanji] = useState('');

	const handleSnackbarClose = (event, reason) => {
		if (reason === 'clickaway') {
			return;
		}
		setSnackbarOpen(false);
	};

	return (
		<MuiThemeProvider theme={theme}>
			<KanjisWrapper>
				<h3>List of Kanji</h3>
				<div style={{ height: '450px' }}>
					<DataGrid autoPageSize={true} rows={rows} columns={columns} />
				</div>

				<Snackbar
					anchorOrigin={{
						vertical: 'bottom',
						horizontal: 'left',
					}}
					open={snackbarOpen}
					autoHideDuration={1500}
					onClose={handleSnackbarClose}
					message={`"${clipboardKanji}" copied to clipboard`}
					action={
						<IconButton size="small" aria-label="close" color="inherit" onClick={handleSnackbarClose}>
							<CloseIcon fontSize="small" />
						</IconButton>
					}
				/>
			</KanjisWrapper>
		</MuiThemeProvider>
	);
}

const KanjisWrapper = styled.div`
	max-width: 1320px;
	margin: 0 auto;
	min-height: calc(100% - 50px);
	display: flex;
	flex-direction: column;
	padding: 16px;
`;

const SearchDiv = styled.div`
	display: flex;
	align-items: center;
`;
