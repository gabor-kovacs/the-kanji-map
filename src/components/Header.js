import React from 'react';
import styled from 'styled-components';
import { NavLink } from 'react-router-dom';

import TableChartIcon from '@material-ui/icons/TableChart';
import HomeIcon from '@material-ui/icons/Home';
import InfoIcon from '@material-ui/icons/Info';

export default function Header() {
	return (
		<HeaderWrapper>
			<ImageLink to="/" exact>
				<div>
					<img src="/assets/images/logo.svg" alt="Logo" height="50px" width="62.66px" />
					<h1>The Kanji Map</h1>
				</div>
			</ImageLink>
			<div>
				<Link
					exact
					activeStyle={{
						color: '#2B99CF',
					}}
					to="/"
				>
					<HomeIcon />
				</Link>
				<Link
					activeStyle={{
						color: '#2B99CF',
					}}
					to="/kanji"
				>
					<TableChartIcon />
				</Link>
				<Link
					to="/about"
					activeStyle={{
						color: '#2B99CF',
					}}
				>
					<InfoIcon />
				</Link>
			</div>
		</HeaderWrapper>
	);
}

// * STYLES **************************************************************************************************

const HeaderWrapper = styled.div`
	height: 50px;
	display: flex;
	align-items: center;
	justify-content: space-between;

	div {
		display: flex;
	}

	img {
		padding: 8px 16px;
		height: 50px;
	}

	h1 {
		display: inline-block;
		white-space: nowrap;
		height: 50px;
		margin: 0;
		line-height: 50px;
		font-size: 1.2rem;
	}
`;

const Link = styled(NavLink)`
	color: #c4c4c4;
	margin-right: 13px;
	margin-left: 8px;
	&:hover {
		color: #dcdcdc;
	}
`;

const ImageLink = styled(NavLink)`
	text-decoration: none;
	color: #212121;
	&:hover {
		color: #212121;
	}
`;
