import React, { useState, useCallback, useEffect, useRef } from 'react';

import { useLocation, useHistory } from 'react-router-dom';

// UTILS
import debounce from 'lodash.debounce';
// COMPONENTS
import SearchAndHistory from './SearchAndHistory';
import DrawInput from './DrawInput';
import Kanji from './Kanji';
import Radical from './Radical';
import Examples from './Examples';
import Graph from './Graph';
// STYLES
import styled from 'styled-components/macro';
import { useSpring, animated } from 'react-spring';

import CircularProgress from '@material-ui/core/CircularProgress';

import useMediaQuery from '@material-ui/core/useMediaQuery';

// A custom hook that builds on useLocation to parse
// the query string for you.
function useQuery() {
	return new URLSearchParams(useLocation().search);
}

export default function App({ chise, data }) {
	// fetching kanji info from JISHO and KanjiAlive
	const [kanjiInfo, setKanjiInfo] = useState('');
	//quickly typed input
	const [input, setInput] = useState('');
	// debounced search term
	const [searchTerm, setSearchTerm] = useState('');
	// current kanji passed down as prop
	const [current, setCurrent] = useState({
		kanji: '',
		structure: [],
		composition: [],
	});

	const [kanjiHistory, setKanjiHistory] = useState(Array(4).fill(null));

	// UPDATE CURRENT KANJI PASSED TO GRAPH
	useEffect(() => {
		chise && setCurrent(chise[searchTerm?.charAt(0)]);
	}, [chise, searchTerm]);

	// Set search term with debounce
	const debounceSetSearchTerm = useCallback(
		debounce((val) => setSearchTerm(val), 500),
		[]
	);

	const handleSearchChange = (e) => {
		const nextValue = e.target.value;
		setInput(nextValue);
		debounceSetSearchTerm(nextValue);
	};

	useEffect(() => {
		// PUSH HISTORY
		if (current?.kanji !== '' && current?.kanji && !kanjiHistory.includes(current?.kanji)) {
			const newKanjiHistory = [...kanjiHistory];
			newKanjiHistory.shift();
			newKanjiHistory.push(current?.kanji);
			setKanjiHistory(newKanjiHistory);
		}
		//SHOW INFO
		data && setKanjiInfo(data[current?.kanji]);
	}, [current]);

	// LISTENING TO RESIZE EVENT /////////////////////////////////
	const [dimensions, setDimensions] = React.useState({
		height: window.innerHeight,
		width: window.innerWidth,
	});
	useEffect(() => {
		const debouncedHandleResize = debounce(function handleResize() {
			setDimensions({
				height: window.innerHeight,
				width: window.innerWidth,
			});
		}, 1000);

		window.addEventListener('resize', debouncedHandleResize);
		return () => {
			window.removeEventListener('resize', debouncedHandleResize);
		};
	});
	//////////////////////////////////////////////////////////////////
	const [drawInputOpen, setDrawInputOpen] = useState(false);

	const [layoutSpring] = useSpring(() => ({
		gridTemplateColumns: '1fr 1fr 1fr',
		gridTemplateRows: '2fr 3fr',
		gridColumnGap: '16px',
	}));

	const [layoutView, setLayoutView] = useState({
		kanjiFocused: false,
		examplesFocused: false,
		radicalFocused: false,
	});

	const focusKanji = () => {
		if (mobile) {
			if (!layoutView.kanjiFocused) {
				layoutSpring.gridTemplateColumns.start('1fr 0fr 0fr');
				layoutSpring.gridTemplateRows.start('2fr 1fr');
				layoutSpring.gridColumnGap.start('0px');
				setLayoutView({
					kanjiFocused: true,
					examplesFocused: false,
					radicalFocused: false,
				});
			}
		}
	};
	const focusExamples = () => {
		if (mobile) {
			if (!layoutView.examplesFocused) {
				layoutSpring.gridTemplateColumns.start('0fr 1fr 0fr');
				layoutSpring.gridTemplateRows.start('2fr 1fr');
				layoutSpring.gridColumnGap.start('0px');
				setLayoutView({
					kanjiFocused: false,
					examplesFocused: true,
					radicalFocused: false,
				});
			}
		}
	};
	const focusRadical = () => {
		if (mobile) {
			if (!layoutView.radicalFocused) {
				layoutSpring.gridTemplateColumns.start('0fr 0fr 1fr');
				layoutSpring.gridTemplateRows.start('2fr 1fr');
				layoutSpring.gridColumnGap.start('0px');
				setLayoutView({
					kanjiFocused: false,
					examplesFocused: false,
					radicalFocused: true,
				});
			}
		}
	};
	const normalView = () => {
		if (mobile) {
			// condition to prevent flicker when interacting with graph
			if (layoutView.kanjiFocused || layoutView.examplesFocused || layoutView.radicalFocused) {
				layoutSpring.gridTemplateColumns.start('1fr 1fr 1fr');
				layoutSpring.gridTemplateRows.start('2fr 3fr');
				layoutSpring.gridColumnGap.start('16px');
				setLayoutView({
					kanjiFocused: false,
					examplesFocused: false,
					radicalFocused: false,
				});
			}
		}
	};

	const mobile = useMediaQuery('(max-width:999px)');
	const smallmobile = useMediaQuery('(max-width:360px)');
	const desktop = useMediaQuery('(min-width:1000px)');

	useEffect(() => {
		// change from mobile to desktop
		if (!mobile && desktop) {
			setDrawInputOpen(true);
		}
		// change from desktop to mobile
		if (mobile && !desktop) {
			setDrawInputOpen(false);
		}
	}, [mobile, desktop]);

	// HISTORY /////////////////////////////////////
	const history = useHistory();
	// READ QUERY IF EXISTS ON FIRST LOAD
	let query = useQuery();
	useEffect(() => {
		query.get('k') && setInput(query.get('k'));
		query.get('k') && debounceSetSearchTerm(query.get('k'));
	}, []);

	// UPDATE ON BACK BUTTON OR HISTORY CHANGE
	useEffect(() => {
		history.listen((location) => {
			let params = new URLSearchParams(document.location.search);
			let k = params.get('k');
			k && setInput(k);
			k && debounceSetSearchTerm(k);
		});
	}, [history]);

	// SET QUERY STRING ON CURRENT UPDATE
	useEffect(() => {
		// SET
		const params = new URLSearchParams();
		if (current) {
			params.append('k', current?.kanji);
		} else {
			params.delete('k');
		}
		history.push({ search: params.toString() });
	}, [current, history]);

	// KANJI INPUT
	const [inputValue, setInputValue] = useState('');
	const inputRef = useRef(null);

	return (
		<>
			<AppWrapper>
				{mobile && !desktop && (
					<>
						<SearchAndHistory
							{...{
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
							}}
						/>
						<MobileLayout style={layoutSpring}>
							<Kanji
								{...{
									setInput,
									debounceSetSearchTerm,
									current,
									kanjiInfo,
									searchTerm,
									focusKanji,
									layoutView,
									normalView,
									mobile,
									desktop,
								}}
							/>
							<Examples {...{ kanjiInfo, focusExamples, layoutView, normalView, mobile }} />
							<Radical {...{ kanjiInfo, focusRadical, layoutView, normalView, mobile }} />
							<Graph
								{...{
									data,
									chise,
									current,
									setInput,
									debounceSetSearchTerm,
									dimensions,
									layoutView,
									normalView,
									mobile,
								}}
							/>
						</MobileLayout>
						<DrawInput {...{ drawInputOpen, setDrawInputOpen, mobile, setInputValue, inputRef }} />
					</>
				)}
				{desktop && !mobile && (
					<DesktopLayout>
						<DesktopLayoutTop>
							<SearchAndHistory
								{...{
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
								}}
							/>
							<DrawInput {...{ drawInputOpen, setDrawInputOpen, mobile, setInputValue, inputRef }} />
							<Kanji
								{...{
									setInput,
									debounceSetSearchTerm,
									current,
									kanjiInfo,
									searchTerm,
									focusKanji,
									layoutView,
									normalView,
									mobile,
									desktop,
								}}
							/>
							<Radical {...{ kanjiInfo, focusRadical, layoutView, normalView, mobile }} />
						</DesktopLayoutTop>
						<DesktopLayoutBottom>
							<Examples {...{ kanjiInfo, focusExamples, layoutView, normalView, mobile }} />
							<Graph
								{...{
									data,
									chise,
									current,
									setInput,
									debounceSetSearchTerm,
									dimensions,
									layoutView,
									normalView,
									mobile,
								}}
							/>
						</DesktopLayoutBottom>
					</DesktopLayout>
				)}
			</AppWrapper>
		</>
	);
}

const AppWrapper = styled.div`
	width: 100%;
	height: 100%;
	overflow: hidden;
`;

const MobileLayout = styled(animated.div)`
	position: absolute;
	top: 100px;
	padding-top: 16px;
	padding-left: 16px;
	padding-right: 16px;
	height: calc(100% - 100px);

	background-color: #fff;
	background-image: linear-gradient(0deg, #ffffff00 0%, #ffffff 90%, #ffffff 100%),
		radial-gradient(
			circle at 100% 150%,
			#2b99cf 24%,
			white 24%,
			white 28%,
			#2b99cf 28%,
			#2b99cf 36%,
			white 36%,
			white 40%,
			transparent 40%,
			transparent
		),
		radial-gradient(
			circle at 0 150%,
			#2b99cf 24%,
			white 24%,
			white 28%,
			#2b99cf 28%,
			#2b99cf 36%,
			white 36%,
			white 40%,
			transparent 40%,
			transparent
		),
		radial-gradient(
			circle at 50% 100%,
			white 10%,
			#2b99cf 10%,
			#2b99cf 23%,
			white 23%,
			white 30%,
			#2b99cf 30%,
			#2b99cf 43%,
			white 43%,
			white 50%,
			#2b99cf 50%,
			#2b99cf 63%,
			white 63%,
			white 71%,
			transparent 71%,
			transparent
		),
		radial-gradient(
			circle at 100% 50%,
			white 5%,
			#2b99cf 5%,
			#2b99cf 15%,
			white 15%,
			white 20%,
			#2b99cf 20%,
			#2b99cf 29%,
			white 29%,
			white 34%,
			#2b99cf 34%,
			#2b99cf 44%,
			white 44%,
			white 49%,
			transparent 49%,
			transparent
		),
		radial-gradient(
			circle at 0 50%,
			white 5%,
			#2b99cf 5%,
			#2b99cf 15%,
			white 15%,
			white 20%,
			#2b99cf 20%,
			#2b99cf 29%,
			white 29%,
			white 34%,
			#2b99cf 34%,
			#2b99cf 44%,
			white 44%,
			white 49%,
			transparent 49%,
			transparent
		);

	background-size: cover, 100px 50px, 100px 50px, 100px 50px, 100px 50px, 100px 50px;

	overflow: hidden;
	display: grid;
	grid-row-gap: 32px;
	grid-template-areas:
		'kanjiArea examplesArea radicalArea'
		'graphArea graphArea graphArea';
`;

const MobileLoading = styled.div`
	position: absolute;
	width: 100%;
	top: 100px;
	height: calc(100% - 100px);

	background-color: #fff;
	display: flex;
	align-items: center;
	justify-content: center;
`;

const DesktopLayout = styled.div`
	position: absolute;
	top: 50px;
	padding-top: 16px;
	padding-left: 16px;
	padding-right: 16px;
	height: calc(100% - 50px);
	/* 
	max-width: 1200px;
	margin: 0 auto; */

	background-color: #fff;
	background-image: linear-gradient(0deg, #ffffff00 0%, #ffffff 90%, #ffffff 100%),
		radial-gradient(
			circle at 100% 150%,
			#2b99cf 24%,
			white 24%,
			white 28%,
			#2b99cf 28%,
			#2b99cf 36%,
			white 36%,
			white 40%,
			transparent 40%,
			transparent
		),
		radial-gradient(
			circle at 0 150%,
			#2b99cf 24%,
			white 24%,
			white 28%,
			#2b99cf 28%,
			#2b99cf 36%,
			white 36%,
			white 40%,
			transparent 40%,
			transparent
		),
		radial-gradient(
			circle at 50% 100%,
			white 10%,
			#2b99cf 10%,
			#2b99cf 23%,
			white 23%,
			white 30%,
			#2b99cf 30%,
			#2b99cf 43%,
			white 43%,
			white 50%,
			#2b99cf 50%,
			#2b99cf 63%,
			white 63%,
			white 71%,
			transparent 71%,
			transparent
		),
		radial-gradient(
			circle at 100% 50%,
			white 5%,
			#2b99cf 5%,
			#2b99cf 15%,
			white 15%,
			white 20%,
			#2b99cf 20%,
			#2b99cf 29%,
			white 29%,
			white 34%,
			#2b99cf 34%,
			#2b99cf 44%,
			white 44%,
			white 49%,
			transparent 49%,
			transparent
		),
		radial-gradient(
			circle at 0 50%,
			white 5%,
			#2b99cf 5%,
			#2b99cf 15%,
			white 15%,
			white 20%,
			#2b99cf 20%,
			#2b99cf 29%,
			white 29%,
			white 34%,
			#2b99cf 34%,
			#2b99cf 44%,
			white 44%,
			white 49%,
			transparent 49%,
			transparent
		);

	/* background-size: cover, 100px 50px, 100px 50px, 100px 50px, 100px 50px, 100px 50px; */
	background-size: cover, 150px 75px, 150px 75px, 150px 75px, 150px 75px, 150px 75px;

	overflow: hidden;
`;

const DesktopLayoutTop = styled.div`
	width: 100%;
	height: 50%;
	display: grid;
	grid-template-areas:
		'searchArea kanjiArea radicalArea'
		'drawArea kanjiArea radicalArea';
	grid-template-columns: 200px 1fr 1fr;
	grid-template-rows: 80px 1fr;
	grid-column-gap: 16px;
	padding-bottom: 16px;
`;

const DesktopLayoutBottom = styled.div`
	width: 100%;
	height: 50%;
	display: grid;
	grid-template-areas: 'examplesArea graphArea';
	grid-template-columns: 2fr 3fr;
	grid-template-rows: 1fr;
	grid-column-gap: 16px;
	padding-bottom: 16px;
`;
