import React, { useState, useEffect, useRef } from 'react';
import ForceGraph3D from 'react-force-graph-3d';

import { joyoList } from '../data/joyo';
import { jinmeiyoList } from '../data/jinmeiyo';

import * as THREE from 'three';
import SpriteText from 'three-spritetext';

import styled from 'styled-components/macro';

export default function Graph(props) {
	const { data, chise, current, setCurrent, dimensions, normalView, mobile } = props;

	const fgRef = useRef();

	const [graphData, setGraphData] = useState({ nodes: [], links: [] });

	// recursively search children nodes
	const findChildren = (array) => {
		array.forEach((el) => {
			chise &&
				chise[el] &&
				chise[el].composition.forEach((node) => {
					if (!array.includes(node)) {
						array.push(node);
						findChildren(array);
					}
				});
		});
		return array;
	};

	const findLinks = (array) => {
		let links = [];
		array.forEach((end) => {
			chise &&
				chise[end] &&
				chise[end].composition.forEach((start) => {
					if (start !== end) {
						links.push({ source: start, target: end });
					}
				});
		});
		return links;
	};

	// find outgoing nodes
	const findParents = () => {
		let parents = [];
		if (chise) {
			for (const [kanji, kanjidata] of Object.entries(chise)) {
				kanjidata.composition.forEach((el) => {
					if (el === current.kanji) {
						// prevent self links
						if (kanjidata.kanji !== current.kanji) {
							parents.push(kanji);
						}
					}
				});
			}
		}
		//remove duplicates
		const parentsRet = [...new Set(parents)];
		return parentsRet;
	};

	const findParentLinks = (parentsArray) => {
		let parentLinks = [];
		if (parentsArray.length > 0 && current.kanji) {
			parentLinks = parentsArray.map((x) => ({ source: current.kanji, target: x }));
		}
		//remove duplicates
		const parentLinksRet = [...new Set(parentLinks)];

		return parentLinksRet;
	};

	useEffect(() => {
		// BUILDING THE GRAPH
		if (current && chise) {
			let Links = [];
			let Nodes = [];
			let outNodes = [];
			let outLinks = [];

			Nodes.push(current.kanji);
			Nodes = findChildren(Nodes);
			Links = findLinks(Nodes);

			outNodes = findParents();
			outLinks = findParentLinks(outNodes);

			Nodes = Nodes.concat(outNodes);
			// remove duplicates
			Nodes = [...new Set(Nodes)];
			Links = Links.concat(outLinks);
			//remove empty string
			Nodes = Nodes.filter((el) => el);

			Nodes = Nodes.map((x) => ({ id: x })); // creating array of objects
			if (outNodes.length > 0) {
				outNodes = outNodes.map((x) => ({ id: x }));
			}

			setGraphData({ nodes: Nodes, links: Links });
		}
	}, [current, chise]);

	function existsFile(url) {
		var http = new XMLHttpRequest();
		http.open('HEAD', url, false);
		http.send();
		return http.status != 404;
	}

	// find same onyomi
	const sameOn = (kanji1, kanji2) => {
		return data[kanji1]?.jishoData?.onyomi?.filter((value) => data[kanji2]?.jishoData?.onyomi?.includes(value));
	};

	// FOCUS  ON MAIN NODE AT START
	useEffect(() => {
		const focusMain = setTimeout(() => {
			if (current?.kanji && graphData?.nodes?.length > 0) {
				let node = graphData?.nodes?.find((o) => o.id === current.kanji);
				const distance = 160;
				const distRatio = 1 + distance / Math.hypot(node.x, node.y, node.z);
				node &&
					fgRef.current.cameraPosition(
						{ x: node.x * distRatio, y: node.y * distRatio, z: node.z * distRatio }, // new position
						node, // lookAt ({ x, y, z })
						1000 // ms transition duration
					);
			}
		}, 1);
		return () => clearTimeout(focusMain);
	}, [graphData]);

	const handleClick = (node) => {
		if (node.id.length <= 4) {
			// setInput(node.id);
			// debounceSetSearchTerm(node.id);
			const kanji = node.id;
			chise && kanji && chise[kanji] && setCurrent(chise[kanji]);
		}
	};

	const handleHover = (node, prevNode) => {
		// TODO: make this more compact
		// RESTORE COLOR OF PREVIOUS HOVERED NODE
		if (prevNode?.id) {
			if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
				prevNode.__threeObj.children[1].material.color.b = 1;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
				prevNode.__threeObj.children[1].material.color.g = 1;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
				prevNode.__threeObj.children[1].material.color.r = 1;
			}
		}

		if (prevNode?.id && joyoList?.includes(prevNode.id)) {
			if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
				prevNode.__threeObj.children[1].material.color.b = 0.8862745098;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
				prevNode.__threeObj.children[1].material.color.g = 0.76078431372;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
				prevNode.__threeObj.children[1].material.color.r = 0.50196078431;
			}
		}

		if (prevNode?.id && jinmeiyoList?.includes(prevNode.id)) {
			if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
				prevNode.__threeObj.children[1].material.color.b = 0.96078431372;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
				prevNode.__threeObj.children[1].material.color.g = 0.92156862745;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
				prevNode.__threeObj.children[1].material.color.r = 0.83529411764;
			}
		}

		if (prevNode?.id && current?.kanji && prevNode.id === current.kanji) {
			if (prevNode?.__threeObj?.children[1]?.material?.color?.b) {
				prevNode.__threeObj.children[1].material.color.b = 0.8117647058823529;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.g) {
				prevNode.__threeObj.children[1].material.color.g = 0.6;
			}
			if (prevNode?.__threeObj?.children[1]?.material?.color?.r) {
				prevNode.__threeObj.children[1].material.color.r = 0.16862745098039217;
			}
		}
		// CHANGE COLOR OF HOVERED NODE
		if (node?.__threeObj?.children[1]?.material?.color?.b) {
			node.__threeObj.children[1].material.color.b = 0.8117647058823529;
		}
		if (node?.__threeObj?.children[1]?.material?.color?.g) {
			node.__threeObj.children[1].material.color.g = 0.6;
		}
		if (node?.__threeObj?.children[1]?.material?.color?.r) {
			node.__threeObj.children[1].material.color.r = 0.16862745098039217;
		}
	};

	return (
		<>
			<GraphWrapper dimensions={dimensions} onClick={normalView}>
				<ForceGraph3D
					width={mobile ? props.dimensions.width : ((props.dimensions.width - 32) * 3) / 5}
					height={mobile ? (props.dimensions.height - 100) * 0.6 : (props.dimensions.height - 50) * 0.5}
					backgroundColor={'#fff'}
					graphData={graphData}
					// ARROWS
					linkColor={() => '#000000'}
					linkDirectionalArrowLength={5}
					linkDirectionalArrowRelPos={0.9}
					linkDirectionalArrowResolution={32}
					//PARTICLES
					linkDirectionalParticles={3}
					linkDirectionalParticleSpeed={0.008}
					linkDirectionalParticleWidth={1}
					linkDirectionalParticleResolution={8}
					enableNodeDrag={false}
					enableNavigationControls={true}
					showNavInfo={false}
					ref={fgRef}
					warmupTicks={100}
					onNodeClick={handleClick}
					onNodeHover={handleHover}
					nodeThreeObject={(node) => {
						let color;
						// if it is he main node
						if (node.id === current?.kanji) {
							color = '#2B99CF';
						} else if (joyoList?.includes(node.id)) {
							color = '#80c2e2';
						} else if (jinmeiyoList?.includes(node.id)) {
							color = '#d5ebf5';
						} else {
							color = '#fff';
						}

						const ball = new THREE.Mesh(
							new THREE.SphereGeometry(8, 32, 32),
							new THREE.MeshLambertMaterial({
								color: color,
								transparent: true,
								depthWrite: false,
								opacity: 0.8,
							})
						);

						// If it's a single character
						if (node.id.length === 1) {
							const sprite = new SpriteText(node.id);
							sprite.fontFace =
								'Iowan Old Style' ||
								'Apple Garamond' ||
								'Baskerville' ||
								'Times New Roman' ||
								'Droid Serif' ||
								'Times' ||
								'Source Serif Pro' ||
								'serif';
							sprite.color = '#000';
							sprite.textHeight = 10;

							const group = new THREE.Group();
							group.add(sprite);
							group.add(ball);
							return group;
						} else {
							let url;
							// OR If it's a single character, but encoded in a a way that it counts as 2-4 characters like an emoji
							if (node.id.length > 1 && node.id.length <= 4) {
								url = '/data/glyphs/' + 'u' + node.id.codePointAt(0).toString(16) + '.svg';
							}
							// OR It might be a unique code that exist in the database
							else {
								url = '/data/glyphs/' + node.id.toLowerCase() + '.svg';
							}
							// IF we found it
							if (existsFile(url)) {
								const textureLoader = new THREE.TextureLoader();
								textureLoader.crossOrigin = 'Anonymous';
								const texture = textureLoader.load(url);
								const material = new THREE.SpriteMaterial({ map: texture });
								const sprite = new THREE.Sprite(material);
								sprite.scale.set(10, 10);
								const group = new THREE.Group();
								group.add(sprite);
								group.add(ball);
								return group;
							}
							// NO glyph founded
							else {
								const sprite = new SpriteText('�');
								sprite.fontFace = 'Noto Serif JP';
								sprite.color = '#000';
								sprite.textHeight = 10;
								const group = new THREE.Group();
								group.add(sprite);
								group.add(ball);
								return group;
							}
						}
					}}
					// ADD ONYOMI TO LINKS
					linkThreeObjectExtend={true}
					linkThreeObject={(link) => {
						const linkText = sameOn(link.source, link.target);
						let sprite;
						if (linkText && linkText.length > 0) {
							sprite = new SpriteText(linkText.join(', '));
						} else {
							return null;
						}
						sprite.color = '#000';
						sprite.textHeight = 5;
						return sprite;
					}}
					linkPositionUpdate={(sprite, { start, end }) => {
						const middlePos = Object.assign(
							...['x', 'y', 'z'].map((c) => ({
								[c]: start[c] + (end[c] - start[c]) / 2, // calc middle point
							}))
						);
						// if there is a same onyomi link
						sprite?.position && Object.assign(sprite.position, middlePos);
					}}
				/>
			</GraphWrapper>
		</>
	);
}

// * STYLES **************************************************************************************************

const GraphWrapper = styled.div`
	grid-area: graphArea;
	width: calc(100% + 32px);
	height: 100%;
	background: #fff;
	padding: 0;
	margin-left: -16px;
	margin-right: -16px;
	box-shadow: 0 10px 20px rgba(0, 0, 0, 0.19), 0 6px 6px rgba(0, 0, 0, 0.23);
	overflow: hidden;

	border-top-left-radius: 40px;
	border-top-right-radius: 40px;

	//fix for ios top border radius
	backface-visibility: hidden;
	transform: translate3d(0, 0, 0);

	@media (min-width: 1000px) {
		width: 100%;
		margin: 0;
		border-radius: 20px;
	}
`;
