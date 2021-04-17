import React, { useEffect } from 'react';
import styled from 'styled-components/macro';

import IconButton from '@material-ui/core/IconButton';
import PlayCircleFilledIcon from '@material-ui/icons/PlayCircleFilled';
import HighlightOffIcon from '@material-ui/icons/HighlightOff';

import { useSpring, animated } from 'react-spring';

export default function Examples({ kanjiInfo, focusExamples, layoutView, normalView, mobile, desktop }) {
	function playSound(url) {
		const audio = new Audio(url);
		audio.play();
	}

	const [springProps] = useSpring(() => ({ opacity: 1, padding: '16px', scale: 0 }));

	useEffect(() => {
		setTimeout(() => {
			springProps.scale.start(1);
		}, 100);
	}, []);

	const closeIconSpringProps = useSpring({
		opacity: layoutView.examplesFocused ? 1 : 0,
		cursor: layoutView.examplesFocused ? 'pointer' : 'default',
	});

	useEffect(() => {
		if (layoutView.kanjiFocused === true || layoutView.radicalFocused === true) {
			springProps.opacity.start(0);
			springProps.padding.start('0px');
		} else {
			springProps.opacity.start(1);
			springProps.padding.start('16px');
		}
	}, [layoutView, mobile, desktop]);

	const handleClose = (e) => {
		e.stopPropagation();
		normalView();
	};

	return (
		<>
			<ExamplesWrapper style={springProps} onClick={focusExamples}>
				<CloseIcon style={closeIconSpringProps}>
					<IconButton
						aria-label="Close"
						size="small"
						style={{ cursor: 'inherit', color: '#c4c4c4' }}
						onClick={handleClose}
					>
						<HighlightOffIcon />
					</IconButton>
				</CloseIcon>
				<h3>Examples</h3>
				{layoutView.examplesFocused || !mobile ? (
					<>
						{/* KANJIALIVE With AUDIO */}
						{kanjiInfo?.kanjialiveData?.examples && <h5>Examples with audio</h5>}
						{kanjiInfo?.kanjialiveData?.examples?.map((example, index) => {
							return (
								<AudioExampleDiv key={index}>
									<p>
										<span>{example?.japanese}&nbsp;&nbsp;&nbsp;</span>
										<span>{example?.meaning?.english}&nbsp;&nbsp;&nbsp;</span>
									</p>
									<IconButton
										aria-label="Play sound"
										size="small"
										onClick={() => playSound(example?.audio?.mp3)}
									>
										<PlayCircleFilledIcon fontSize="small" />
									</IconButton>
								</AudioExampleDiv>
							);
						})}
						{/* JISHO */}
						{kanjiInfo?.jishoData?.onyomiExamples && kanjiInfo?.jishoData?.onyomiExamples?.length !== 0 && (
							<h5>Onyomi Examples</h5>
						)}
						{kanjiInfo?.jishoData?.onyomiExamples?.map((onExample, index) => (
							<p key={index}>
								{onExample?.example}&nbsp;&nbsp;（{onExample?.reading}）&nbsp;&nbsp;&nbsp;
								{onExample?.meaning}
							</p>
						))}

						{kanjiInfo?.jishoData?.kunyomiExamples &&
							kanjiInfo?.jishoData?.kunyomiExamples?.length !== 0 && <h5>Kunyomi Examples</h5>}
						{kanjiInfo?.jishoData?.kunyomiExamples?.map((kunExample, index) => (
							<p key={index}>
								{kunExample?.example}&nbsp;&nbsp;（{kunExample?.reading}）&nbsp;&nbsp;&nbsp;
								{kunExample?.meaning}
							</p>
						))}
					</>
				) : (
					<>
						{kanjiInfo?.jishoData?.onyomiExamples?.map((onExample, index) => {
							if (index < 3) {
								return <p key={index}>{onExample?.example}</p>;
							}
						})}
						{kanjiInfo?.jishoData?.onyomiExamples && kanjiInfo?.jishoData?.onyomiExamples?.length !== 0 && (
							<p>...</p>
						)}
					</>
				)}
			</ExamplesWrapper>
		</>
	);
}

// * STYLES **************************************************************************************************

const ExamplesWrapper = styled(animated.div)`
	grid-area: examplesArea;
	position: relative;
	width: 100%;
	height: 100%;
	border-radius: 20px;
	background: white;
	box-shadow: 0 10px 20px rgba(0, 0, 0, 0.19), 0 6px 6px rgba(0, 0, 0, 0.23);
	overflow: auto;

	h3 {
		font-size: 15px;
		margin: 0;
		text-align: center;
		padding-bottom: 16px;
		@media (max-width: 374px) {
			font-size: 11px;
		}
		@media (min-width: 1000px) {
			width: 100px;
		}
	}

	h5 {
		margin: 10px 0;
	}
	p {
		font-size: 15px;
		line-height: 22px;
		margin: 0;
		padding-bottom: 5px;
	}
`;

const CloseIcon = styled(animated.div)`
	position: absolute;
	top: 0;
	right: 0;
	padding: 4px;
`;

const AudioExampleDiv = styled.div`
	display: flex;
	justify-content: space-between;
	align-items: flex-end;
`;
