import React, { useState, useEffect } from 'react';

import styled from 'styled-components/macro';
import TextField from '@material-ui/core/TextField';
import IconButton from '@material-ui/core/IconButton';
import CreateIcon from '@material-ui/icons/Create';

import { createMuiTheme, ThemeProvider as MuiThemeProvider } from '@material-ui/core/styles';

import { makeStyles } from '@material-ui/core/styles';

import Autocomplete from '@material-ui/lab/Autocomplete';
import { FixedSizeList } from 'react-window';
import Popper from '@material-ui/core/Popper';

function renderRow(props) {
	const { data, index, style } = props;
	return React.cloneElement(data[index], {
		style: {
			overflow: 'hidden',
			textOverflow: 'ellipsis',
			whiteSpace: 'nowrap',
			display: 'block',
			...style,
		},
	});
}
const PopperComponent = function (props) {
	// const { mobile } = props;

	return (
		<Popper
			{...props}
			// style={{ width: 'calc(100vw - 32px)' }}
			style={{ width: '200px' }}
			// style={{ width: mobile ? '200px' : '800px' }}
			placement="bottom-start"
		/>
	);
};

const ListboxComponent = React.forwardRef(function ListboxComponent(props, ref) {
	const { children, ...other } = props;
	const itemCount = Array.isArray(children) ? children.length : 0;
	const itemSize = 32;

	const outerElementType = React.useMemo(() => {
		return React.forwardRef((props2, ref2) => <div ref={ref2} {...props2} {...other} />);
	}, []); // eslint-disable-line react-hooks/exhaustive-deps

	return (
		<div ref={ref}>
			<FixedSizeList
				style={{
					padding: 0,
					height: Math.min(10, itemCount) * itemSize,
					maxHeight: 'auto',
				}}
				itemData={children}
				height={180}
				width="100%"
				// width={300}
				outerElementType={outerElementType}
				innerElementType="ul"
				itemSize={itemSize}
				overscanCount={5}
				itemCount={itemCount}
			>
				{renderRow}
			</FixedSizeList>
		</div>
	);
});

const useStyles = makeStyles({
	listbox: {
		'& ul': {
			padding: 0,
			margin: 0,
			paddingTop: '100px',
		},
	},
});

export default function SearchAndHistory(props) {
	const {
		data,
		kanjiHistory,
		drawInputOpen,
		setDrawInputOpen,
		mobile,
		smallmobile,
		setCurrent,
		chise,
		inputValue,
		setInputValue,
		inputRef,
	} = props;

	const classes = useStyles();

	const selectHistory = (e) => {
		const kanji = e?.currentTarget?.value;
		chise && kanji && chise[kanji] && setCurrent(chise[kanji]);
	};

	const [options, setOptions] = useState(null);

	useEffect(() => {
		if (data) {
			let newOptions = [];
			for (const [kanji, kanjidata] of Object.entries(data)) {
				if (kanji.length <= 4) {
					newOptions.push({
						id: kanji,
						meaning: kanjidata?.jishoData?.meaning !== undefined ? kanjidata?.jishoData?.meaning : '',
					});
				}
			}
			setOptions(newOptions);
		}
	}, [data]);

	const handleInputSelect = (selected) => {
		chise && selected?.id && chise[selected.id] && setCurrent(chise[selected.id]);
	};

	// * LIST

	return (
		<MuiThemeProvider theme={inputTheme}>
			<SearchAndHistoryWrapper>
				<SearchDiv>
					{options && (
						<Autocomplete
							classes={classes}
							disableListWrap
							ListboxComponent={ListboxComponent}
							PopperComponent={PopperComponent}
							openOnFocus={true}
							options={options}
							getOptionLabel={(option) =>
								`${option.id} 
						${option.meaning}`
							}
							renderInput={(params) => (
								<TextField
									{...params}
									inputRef={inputRef}
									style={{ width: mobile ? (smallmobile ? '135px' : '160px') : '200px' }}
									color="primary"
									label="Search"
									variant="outlined"
									size="small"
								/>
							)}
							onChange={(e, newValue) => {
								handleInputSelect(newValue);
							}}
							inputValue={inputValue}
							onInputChange={(e, newInputValue) => {
								setInputValue(newInputValue);
							}}
						/>
					)}
					{mobile && (
						<IconButton
							className="openDrawInput"
							aria-label="Draw Input"
							size="small"
							style={{ color: drawInputOpen ? '#2B99CF' : '#c4c4c4', marginLeft: '5px' }}
							onClick={() => setDrawInputOpen(!drawInputOpen)}
						>
							<CreateIcon />
						</IconButton>
					)}
				</SearchDiv>

				<HistoryDiv>
					{/* <p>recent: </p> */}
					{kanjiHistory
						.filter((e) => e)
						.map((historyElement, index) => (
							<IconButton
								key={index}
								value={historyElement}
								aria-label={historyElement}
								size="small"
								style={{ fontSize: 16, padding: 8 }}
								onClick={(e) => selectHistory(e)}
							>
								{historyElement}
							</IconButton>
						))}
				</HistoryDiv>
			</SearchAndHistoryWrapper>
		</MuiThemeProvider>
	);
}

// * STYLES **************************************************************************************************

const SearchAndHistoryWrapper = styled.div`
	height: 50px;
	max-width: 1000px;
	margin: 0 auto;
	padding: 0 16px;
	display: flex;
	align-items: center;
	justify-content: flex-start;

	p {
		margin: 0;
		line-height: 30px;
		padding-left: 10px;
		padding-right: 10px;
	}

	@media (min-width: 1000px) {
		height: 100px;
		flex-direction: column;
		width: 200px;
		padding: 0;
		margin: 0;

		p {
			padding-left: 4px;
		}
	}
`;

const HistoryDiv = styled.div`
	width: 100%;
	display: flex;
	align-items: center;
	justify-content: flex-start;
	@media (min-width: 1000px) {
		justify-content: center;
	}
`;

const inputTheme = createMuiTheme({
	palette: {
		primary: {
			main: '#2B99CF',
		},
	},
});

const SearchDiv = styled.div`
	display: flex;
	align-items: center;
`;
