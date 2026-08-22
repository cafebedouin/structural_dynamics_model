% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__portuguese_exploration_legitimation, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tordesillas_demarcation_kernel__portuguese_exploration_legitimation
 *   human_readable: Tordesillas Treaty — Portuguese Exploration Legitimation Reading
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1494 Treaty of Tordesillas, ratified by the papal bull Eximiae
 *   Devotionis (1506), established a meridian 370 leagues west of the Cape
 *   Verde islands dividing the extra-European world between the Portuguese
 *   and Spanish crowns. This constraint story instantiates the PORTUGUESE
 *   EXPLORATION LEGITIMATION reading: the treaty as confirmation of
 *   Portugal's prior exploration rights (the Cape Route, the African coast,
 *   the Indian Ocean) and as legal title to exclude rival European powers
 *   (Castile, France, England, Netherlands, Hanseatic League) from the
 *   eastern hemisphere. The extraction is the trade monopoly rent — the
 *   Estado da Índia's cartel pricing on spices and textiles enforced by the
 *   cartaz system and naval violence. The victims are rival European crowns
 *   and merchant networks denied access; indigenous polities are excluded
 *   from the treaty's party structure entirely (a separate constraint). The
 *   claimed type is tangled_rope: genuine coordination (preventing Iberian
 *   war) plus asymmetric extraction (Portuguese trade monopoly enforced
 *   against other Europeans), requiring active enforcement (naval patrols,
 *   cartaz issuance, diplomatic pressure). The founding problem (war
 *   prevention) died by 1530; the constraint persisted as a monopoly
 *   instrument — a candidat for mandatrophy reclassification to piton.
 *
 * KEY AGENTS:
 *   - portuguese_estado_da_india: Primary beneficiary (institutional/arbitrage) — collects monopoly rents, enforces exclusion
 *   - portuguese_crown: Agenda-setter/beneficiary (institutional/arbitrage) — negotiated treaty, administers patronage, collects quinto
 *   - portuguese_merchant_houses: Beneficiary (organized/mobile) — finance armadas, distribute spices, bear voyage risk
 *   - castilian_crown: Primary payer (institutional/constrained) — excluded east of line, probed Moluccas/Philippines, paid 350k ducats at Zaragoza
 *   - french_crown: Payer (institutional/mobile) — never recognized treaty, corsairs and navigators ignored line
 *   - english_crown: Payer (institutional/mobile) — never ratified, privateers and EIC targeted Portuguese monopoly
 *   - dutch_republic: Payer (organized/mobile) — inherited revolt against Iberian Union, VOC broke monopoly by force
 *   - hanseatic_merchant_alliances: Payer (organized/constrained) — cut out of spice distribution, petitioned but lacked naval power
 *   - papal_curia: Agenda-setter/observer (institutional/analytical) — issued bulls, adjudicated legitimacy, benefited from petitioner flow
 *   - indigenous_polities_indian_ocean: Excluded (organized/constrained) — sovereigns of the territory, no voice in treaty, experienced enforcement as violence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58).
domain_priors:suppression_score(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.72).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, extractiveness, 0.58).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tangled_rope).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "Tordesillas Treaty — Portuguese Exploration Legitimation Reading").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__portuguese_exploration_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'f2909bae-d81c-4da5-b8f6-c15ffb361ebc').
narrative_ontology:cs_kernel_codification('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', formalized).
narrative_ontology:cs_authority_grounding('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', lineage).
narrative_ontology:cs_interpretation_layer_present('f2909bae-d81c-4da5-b8f6-c15ffb361ebc').
narrative_ontology:cs_reading_relation('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', tordesillas_demarcation_kernel__spanish_conquest_legitimation, coexists_with).
narrative_ontology:cs_axiom('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', foundational, prior_discovery_creates_exclusive_rights).
narrative_ontology:cs_axiom_status(prior_discovery_creates_exclusive_rights, holdable).
narrative_ontology:cs_axiom_grounding('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', prior_discovery_creates_exclusive_rights, conventional).
narrative_ontology:cs_axiom('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', foundational, papal_arbitration_binds_christian_powers_only).
narrative_ontology:cs_axiom_status(papal_arbitration_binds_christian_powers_only, holdable).
narrative_ontology:cs_axiom_grounding('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', papal_arbitration_binds_christian_powers_only, conventional).
narrative_ontology:cs_reference_frame('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', iberian_peace_under_papal_arbitration).
narrative_ontology:cs_drift_state('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', post_zaragoza_1529, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f2909bae-d81c-4da5-b8f6-c15ffb361ebc', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchant_houses).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_republic).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, hanseatic_merchant_alliances).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, prior_discovery_creates_sovereign_rights).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_arbitration_binds_christian_powers).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, meridian_demarcation_is_legitimate_ordering).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Portuguese Estado da Índia (the crown's Asian trading enterprise) holds the operational monopoly on the Cape Route and the Indian Ocean trade system. It collects the customs duties, enforces the cartaz system (safe-conduct passes), and operates the fortified feitorias (trading posts) from Mozambique to Malacca to Macau. The Tordesillas line, as interpreted by Lisbon, legitimizes its exclusion of rival European vessels from the eastern hemisphere. Its exit is arbitrage-grade: it can redirect capital to Atlantic sugar, Brazilian dyewood, or African gold if the Asian monopoly erodes, and its institutional memory treats the treaty as a negotiable instrument rather than a sacred boundary.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_estado_da_india, agenda_setter).

% The Portuguese Crown (João II, Manuel I, João III, Sebastião) negotiated the treaty, secured papal ratification (Inter Caetera, Eximiae Devotionis), and administers the patronage system that staffs the Estado da Índia. It collects the quinto (royal fifth) on all eastern trade and uses the treaty as the legal basis for demanding obedience from other Christian princes. Its exit is arbitrage: the crown's Atlantic possessions (Brazil, Azores, Madeira, Guinea coast) provide alternative revenue streams, and its diplomatic corps can pivot to alliance with France or England against Spain when the treaty becomes inconvenient.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_crown, beneficiary).

% Lisbon and Porto merchant houses (the Marchionni, Affaitadi, Gouvêa, and later the German and Italian factor communities) finance the armadas, insure the cargoes, and distribute the spices in Antwerp, Nuremberg, and Venice. They benefit from the cartel pricing the treaty enables but bear the capital risk of the voyage. Their exit is mobile: they can shift capital to Atlantic trades, Mediterranean commerce, or banking if the eastern monopoly becomes unprofitable — many did so after 1570 when the Antwerp market collapsed.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_merchant_houses, beneficiary,
    organized, biographical, mobile, global).

% The Castilian Crown (Ferdinand & Isabella, Charles V, Philip II) is the primary European rival excluded east of the line. It accepted the treaty under duress (papal pressure, need for legitimacy in the Americas) but never treated the eastern hemisphere as permanently closed. Its conquistadors and navigators (Magellan, Loaísa, Saavedra, Villalobos) repeatedly probed the Moluccas and the Philippines. Its exit is constrained: it cannot openly reject the papal arbitration without schism risk, but it exploits the treaty's ambiguity (where exactly is the line? does it apply to the antipodes?) to press claims. The 1529 Zaragoza settlement (350,000 ducats for the Moluccas claim) is the priced exit.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, castilian_crown, payer,
    institutional, generational, constrained, global).

% The French Crown (Francis I, Henry II, Charles IX) rejected the treaty's legitimacy from the start (Francis I's famous demand to see 'the clause in Adam's will'). Its navigators (Verrazzano, Cartier, Roberval) and corsairs operated in the Atlantic and Indian Ocean without recognizing Portuguese exclusivity. Its exit is mobile: it never accepted the constraint as binding, so its cost is the diplomatic friction and occasional naval conflict, not compliance. It extracts value by ignoring the line where Portuguese enforcement is weak (Sumatra, Borneo, Japan).
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, french_crown, payer,
    institutional, biographical, mobile, global).

% The English Crown (Henry VII, Henry VIII, Elizabeth I) similarly never ratified Tordesillas. Its merchants (the Muscovy Company, the East India Company charter 1600) and privateers (Drake, Cavendish, Fenton) treated the Portuguese monopoly as a target, not a law. Its exit is mobile: the constraint is not structurally binding on England; the cost is the risk of Portuguese naval retaliation and the lack of safe ports east of the Cape until the EIC establishes its own.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, english_crown, payer,
    institutional, biographical, mobile, global).

% The Dutch Republic (after 1581) inherits the revolt against Habsburg Spain and with it the Portuguese monopoly (Iberian Union 1580–1640). Its companies (predecessors of the VOC) and admirals (Houtman, Van Neck) treat the Tordesillas line as a Spanish-Portuguese fiction to be broken by force and superior capital. Its exit is mobile: it never recognized the constraint; its cost is the war risk, which it prices into the venture capital model.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dutch_republic, payer,
    organized, biographical, mobile, global).

% The Hanseatic merchants of Lübeck, Hamburg, Danzig, and Riga were the established distributors of Portuguese spices in Northern Europe before the treaty. The Portuguese crown's shift to direct sales in Antwerp (and later the Portuguese embargo on Hanseatic shipping) cut them out of the supply chain. Their exit is constrained: they lack naval power to challenge the blockade, and their overland distribution network is locked to the Baltic-North Sea axis. They petition imperial diets and negotiate with Lisbon, but their structural position is payer without leverage.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, hanseatic_merchant_alliances, payer,
    organized, biographical, constrained, regional).

% The Papal Curia (Alexander VI, Julius II, Leo X, Paul III) issued the bulls (Inter Caetera, Dudum Siquidem, Eximiae Devotionis) that ground the treaty's legitimacy. It benefits from the petitioner status of both Iberian crowns and the flow of dispensations, crusading taxes, and missionary patronage. Its exit is analytical: it stands outside the material extraction, adjudicating the legal form. Its interest is the preservation of papal arbitration as a binding mechanism among Christian powers — if the treaty fails, papal authority over the extra-European world fails with it.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, agenda_setter,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, papal_curia, observer).

% The Sultanates of Gujarat, Bijapur, Ahmednagar, Aceh, Demak, Ternate, Tidore, and the Zamorin of Calicut — the sovereign polities whose ports and trade networks the Portuguese inserted themselves into — were not parties to Tordesillas and had no voice in its negotiation. They experience the treaty's enforcement as Portuguese naval violence and cartel pricing. Their exit is constrained: they can resist militarily (Aceh, Gujarat, Calicut did), form anti-Portuguese coalitions, or accommodate, but they cannot appeal to the treaty's authority because they are outside the Christian commonwealth it presupposes. They are the absent sovereigns whose exclusion is the treaty's silent premise.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, indigenous_polities_indian_ocean, excluded,
    organized, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a papal-arbitrated meridian line that allocates exclusive rights of navigation, trade, and settlement between the two Iberian crowns, preventing open war over the Atlantic discoveries and creating a legal framework for papal oversight of the extra-European world.
% TRANSFER_FUNCTION: Moves the right to exclude rival European vessels from the eastern hemisphere (and the resulting monopoly rents on pepper, cloves, nutmeg, cinnamon, and textiles) from the open-access baseline to the Portuguese Estado da Índia, enforced by Portuguese naval power and papal excommunication threat.
% ABSENT_VOICES: The indigenous sovereign polities of the Indian Ocean and Maritime Southeast Asia (Calicut, Gujarat, Aceh, Ternate, Tidore, etc.) are structurally excluded — they are the territories over which the line allocates rights, not parties to the allocation. Their exclusion is not incidental; the treaty's logic requires that only Christian princes can hold valid title.
% DISAPPEARANCE_RATIONALE: If the Portuguese legitimation reading vanished overnight, the legal basis for Portuguese exclusion of other European powers from the Indian Ocean would collapse. The Cape Route would become an open competitive arena; the cartaz system would lose its legal foundation; the Estado da Índia's revenue model would shift from monopoly rents to competitive trade; and the papal arbitration system for extra-European disputes would lose its founding precedent.
% FOUNDING_PROBLEM: After Columbus's 1492 voyage, the Spanish and Portuguese crowns faced imminent conflict over competing claims to the Atlantic discoveries. Portugal needed to protect its African-Guinea trade and the anticipated Cape Route to India; Spain needed legitimacy for its western route. The papal arbitration was the only mechanism both recognized as binding.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (preventing Iberian war over Atlantic discoveries) was resolved by the treaty itself and later by the Iberian Union (1580–1640). The constraint persisted long after the war-prevention function lapsed, repurposed as the legal title for Portuguese trade monopoly. Corroboration: Portuguese chroniclers (Barros, Couto, Góis) document the treaty's negotiation as war-avoidance; Spanish historians (Herrera, Oviedo) confirm the same; modern diplomatic historians (Diffie & Winius, Boxer, Subrahmanyam) concur the anti-war function was exhausted by 1530 while the monopoly function intensified.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__portuguese_exploration_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate: the Portuguese monopoly extracts significant rent from European rivals and Asian producers, but the constraint also solves a real coordination problem (Iberian peace) and the extraction is partially the cost of maintaining the Cape Route infrastructure. Suppression (0.72) is high: the constraint's persistence depends on active naval enforcement (armadas, carracks, forts), the cartaz licensing system, and diplomatic pressure on rival courts — not on voluntary compliance. Theater ratio (0.38) is moderate and rising: the 'crusade and civilization' rhetoric increasingly covers the commercial monopoly as the founding peace function dies. Accessibility collapse (0.61) reflects that once the papal arbitration is accepted, alternative legal frameworks (open seas, free trade) are structurally excluded for Christian powers. Resistance (0.54) is moderate: rival European powers resist by ignoring the line (France, England, Netherlands) or by probing its ambiguities (Castile at the Moluccas), but indigenous polities' resistance is a separate constraint.
 *
 * PERSPECTIVAL GAP:
 *   The Portuguese Estado da Índia and Crown experience the constraint as legitimate coordination they built and maintain (d near beneficiary end). The Castilian Crown experiences it as a binding but exploitable legal fence (d mid-target, constrained exit). The French, English, and Dutch Crowns experience it as an illegitimate claim they violate at will (d near target but mobile exit — they never accepted the constraint). The Hanseatic merchants experience it as a blockade they cannot break (d high target, constrained exit). The Papal Curia experiences it as a legal instrument whose authority must be preserved (analytical seat). Indigenous polities experience it as violent insertion into their sovereign trade networks (excluded, not coordinated). The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: Portuguese Estado da Índia (collects monopoly rents, controls enforcement), Portuguese Crown (negotiated treaty, collects royal fifth), Portuguese merchant houses (finance and distribute at cartel prices). Victims: Castilian Crown (excluded from Moluccas/Philippines, paid Zaragoza indemnity), French Crown (denied legal access, operates as corsair), English Crown (denied legal access, operates as privateer), Dutch Republic (denied legal access, breaks monopoly by force), Hanseatic merchants (cut out of distribution, no naval recourse). The papal bulls create the legal form; Portuguese naval power creates the enforcement. Indigenous polities are not victims of THIS constraint — they are the territory over which the European constraint operates; their subjugation is the cartaz/feitoria system, a separate but linked constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (preventing Iberian war over Atlantic discoveries) was resolved by the treaty itself and rendered obsolete by the Iberian Union (1580). The constraint persisted for 86 years after the founding problem died, repurposed as the legal title for Portuguese trade monopoly against non-Iberian Europeans. The rising theater_ratio (0.22→0.38) and stable extractiveness (0.55→0.58) with stable suppression (0.71→0.72) indicate a constraint whose coordination function has atrophied but whose extraction function persists — the signature of a piton candidate. However, the constraint still coordinates an inter-European trade order (the 'Portuguese century' in the Indian Ocean is a real coordination outcome, however extractive), so tangled_rope remains the claimed type. The mandatrophy_analysis omega documents this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_as_law_vs_cover,
    'Is the Tordesillas line a genuine coordination mechanism that prevented war between Iberian powers, or a cover story for Portuguese trade monopoly that the papal bulls legitimized ex post?',
    'Counterfactual analysis: would Portugal have enforced the Cape Route monopoly without the treaty? Compare Portuguese actions in areas where the treaty did not apply (e.g., West Africa before 1494, Brazil after 1500) to see if the monopoly pattern is treaty-dependent or prior.',
    'If cover story, the constraint is a snare with papal blessing; if genuine coordination that later accumulated extraction, it is a tangled_rope with a dead founding problem. The classification hinges on whether the coordination function was real at t=0.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_as_law_vs_cover, conceptual, 'Whether the treaty''s coordination function is structural or rhetorical.').

omega_variable(
    indigenous_sovereignty_exclusion,
    'Does the exclusion of indigenous polities from the treaty''s party structure constitute a structural feature of the constraint (the constraint only governs European-Christian relations) or a suppression mechanism that enables extraction by denying standing to the actual sovereigns of the territory?',
    'Trace the legal genealogy: does the Portuguese crown ever treat indigenous rulers as sovereign equals in treaty negotiations (e.g., 1500s treaties with Cochin, Cannanore, Ternate)? If yes, the exclusion from Tordesillas is a specific legal choice, not a structural necessity.',
    'If suppression mechanism, the constraint''s victim set expands to include indigenous polities and the extraction is higher; if structural feature, the constraint is genuinely inter-European and the indigenous experience is a separate constraint (the cartaz system, the feitoria system).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_sovereignty_exclusion, conceptual, 'Whether indigenous exclusion is a design parameter or a suppression device.').

omega_variable(
    reading_relation_frustration,
    'Does the Portuguese exploration legitimation reading foreclose, coexist with, or influence the Spanish conquest legitimation reading? The kernel (the treaty text and papal bulls) is shared; the readings differ on whether the grant is confirmation of prior rights (Portugal) or license for future conquest (Spain).',
    'Analyze the legal arguments each crown made at Valladolid (1512), Badajoz-Elvas (1524), and Zaragoza (1529). Did Portuguese jurists argue that the Spanish reading was logically impossible under the same text (forecloses), or did they argue that the Spanish reading was a misapplication that could be corrected by proper interpretation (coexists_with)? Did the Portuguese reading create structural pressure on the Spanish reading by establishing effective possession east of the line (influences)?',
    'Determines the reading_relations entry in cs_structure and whether the kernel is a site of genuine logical conflict or parallel instrumentalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_frustration, conceptual, 'Structural relationship between the two declared readings of the Tordesillas kernel.').

omega_variable(
    mandatrophy_of_war_prevention,
    'The founding problem (preventing Iberian war) died by 1530. The constraint persisted until 1580 (Iberian Union) and its legal effects until 1750 (Treaty of Madrid). Is the constraint''s post-1530 persistence pure mandatrophy (inertial survival of a dead function) or does the trade monopoly constitute a new live function that the treaty legitimizes?',
    'Measure the treaty''s citation frequency in Portuguese royal decrees, papal briefs, and diplomatic correspondence 1530–1580. If citations shift from ''peace with Castile'' to ''exclusion of French/English/Dutch'', the function has mutated, not atrophied.',
    'If mutatated function, the constraint is a tangled_rope with a live coordination function (inter-European trade order); if pure mandatrophy, it is a piton (theatrical maintenance of a dead peace treaty). The theater_ratio trajectory (rising 0.22→0.38) suggests the latter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_war_prevention, empirical, 'Whether the constraint''s post-founding persistence is functional mutation or inertial survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 1494, 1580).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1494, 0.22).
narrative_ontology:measurement(tord_tr_t1500, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1500, 0.28).
narrative_ontology:measurement(tord_tr_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1510, 0.32).
narrative_ontology:measurement(tord_tr_t1525, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1525, 0.35).
narrative_ontology:measurement(tord_tr_t1540, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1540, 0.37).
narrative_ontology:measurement(tord_tr_t1560, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1560, 0.38).
narrative_ontology:measurement(tord_tr_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, theater_ratio, 1580, 0.38).

% Extraction over time
narrative_ontology:measurement(tord_be_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1494, 0.42).
narrative_ontology:measurement(tord_be_t1500, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1500, 0.48).
narrative_ontology:measurement(tord_be_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1510, 0.52).
narrative_ontology:measurement(tord_be_t1525, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1525, 0.55).
narrative_ontology:measurement(tord_be_t1540, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1540, 0.57).
narrative_ontology:measurement(tord_be_t1560, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1560, 0.58).
narrative_ontology:measurement(tord_be_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, base_extractiveness, 1580, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1494, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1494, 0.65).
narrative_ontology:measurement(tord_su_t1500, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement(tord_su_t1510, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1510, 0.7).
narrative_ontology:measurement(tord_su_t1525, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1525, 0.71).
narrative_ontology:measurement(tord_su_t1540, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1540, 0.72).
narrative_ontology:measurement(tord_su_t1560, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1560, 0.72).
narrative_ontology:measurement(tord_su_t1580, tordesillas_demarcation_kernel__portuguese_exploration_legitimation, suppression_requirement, 1580, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, resource_allocation).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, 0.18).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, tordesillas_demarcation_kernel__spanish_conquest_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_cartaz_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, portuguese_feitoria_network).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, zaragoza_treaty_1529).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, iberian_union_1580).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, treaty_of_madrid_1750).

% DUAL FORMULATION NOTE:
% This constraint and spanish_conquest_legitimation are dual readings of the Tordesillas kernel. This reading treats the meridian as a trade-monopoly boundary (resource_allocation coordination type, moderate extractiveness, European victims). The sibling reading treats it as a conquest-license boundary (enforcement_mechanism or identity_coordination type, higher extractiveness, indigenous victims). They share the same legal text but instantiate different constraints with different ε, different victim sets, and different coordination functions. The kernel's ambiguity (is the grant confirmatory or constitutive? does it allocate sovereignty or navigation rights?) is what enables the dual formulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, institutional, 0.15).
constraint_indexing:directionality_override(tordesillas_demarcation_kernel__portuguese_exploration_legitimation, organized, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
