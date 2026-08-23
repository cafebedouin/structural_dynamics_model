% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Concession License for Western Conquest and Indigenous Subjugation (Spanish Reading)
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   In 1493–1494 a papal donation and the subsequent Iberian treaty drew a
 *   north–south line through the Atlantic and conferred on the Castilian
 *   crown all non-Christian lands west of it. Under this arrangement the
 *   grant operates not as a navigation charter but as a title to dominion: it
 *   licenses armed entry, political subjugation, compulsory conversion, and
 *   the appropriation of labor and wealth from the peoples living west of the
 *   line. The license was operationalized through capitulaciones sold to
 *   private commanders, the Requerimiento's formal demand for submission, and
 *   the encomienda — grants of indigenous labor and tribute to settler
 *   households justified as payment for protection and religious instruction.
 *   Enforcement ran through viceroys, audiencias, garrison towns, and a
 *   colonial inquisition; extraction peaked in draft labor for the silver and
 *   mercury mines. Resistance was continuous, from Antillean risings through
 *   the Mixtón, Arauco, and Pueblo wars; internal scholastic challenge forced
 *   the crown to pause, regulate, and finally (Ordinances of 1573) retire
 *   private conquest as a licensed enterprise — while leaving the extractive
 *   infrastructure it had authorized intact. KEY AGENTS (by structural
 *   relationship): - [spanish_crown_and_council_of_indies]: Agenda-setter and
 *   principal beneficiary (institutional/arbitrage) — issues, prices, and
 *   revokes the license; collects the royal fifth -
 *   [encomenderos_and_settlers]: Direct capturer of extraction
 *   (organized/mobile) — hold grants of indigenous labor and tribute -
 *   [mendicant_orders_and_colonial_church]: Secondary beneficiary
 *   (institutional/identity_locked) — runs doctrinas and parishes; houses the
 *   arrangement's fiercest internal critics - [antillean_taino_communities]:
 *   First-wave target (powerless/trapped) -
 *   [mesoamerican_altepetl_communities]: Conquest-phase target
 *   (organized/constrained) - [andean_ayllu_communities]: Consolidation-phase
 *   target (organized/trapped) - [cacicazgo_kuraka_intermediaries]: Co-opted
 *   intermediary — pays upward, collects downward (moderate/constrained) -
 *   [atlantic_rival_maritime_powers]: Excluded — never recognize line or
 *   grant (powerful/arbitrage) - [dominican_scholastic_critics]: Analytical
 *   seat inside the church (institutional/identity_locked)
 *
 * KEY AGENTS:
 *   - spanish_crown_and_council_of_indies — agenda-setter and principal beneficiary (institutional/arbitrage): issues, prices, and revokes the license; collects the royal fifth
 *   - encomenderos_and_settlers — direct capturer of extraction (organized/mobile): hold grants of indigenous labor and tribute; execute collection with armed households
 *   - mendicant_orders_and_colonial_church — secondary beneficiary (institutional/identity_locked): runs the conversion apparatus; houses the arrangement's fiercest internal critics
 *   - antillean_taino_communities — first-wave target (powerless/trapped): earliest labor drafts, requisitions, and collapse; risings broken by divide-and-resettle
 *   - mesoamerican_altepetl_communities — conquest-phase target (organized/constrained): tribute assessments, land loss, congregación resettlement, co-opted nobility
 *   - andean_ayllu_communities — consolidation-phase target (organized/trapped): encomienda then state mine drafts through co-opted kurakas
 *   - cacicazgo_kuraka_intermediaries — co-opted intermediary, payer above and collector below (moderate/constrained): retains rank by administering the exaction
 *   - atlantic_rival_maritime_powers — excluded (powerful/arbitrage): never recognize line or grant; operate wholly outside the arrangement
 *   - dominican_scholastic_critics — analytical seat inside the church (institutional/identity_locked): deny the title's validity; win arguments, hold no lever
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.86).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.85).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.86).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Concession License for Western Conquest and Indigenous Subjugation (Spanish Reading)").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'bca73720-ea9f-4e34-9037-50e52b0a411a').
narrative_ontology:cs_kernel_codification('bca73720-ea9f-4e34-9037-50e52b0a411a', fixed_text).
narrative_ontology:cs_authority_grounding('bca73720-ea9f-4e34-9037-50e52b0a411a', lineage).
narrative_ontology:cs_interpretation_layer_present('bca73720-ea9f-4e34-9037-50e52b0a411a').
narrative_ontology:cs_reading_relation('bca73720-ea9f-4e34-9037-50e52b0a411a', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, influences).
narrative_ontology:cs_axiom('bca73720-ea9f-4e34-9037-50e52b0a411a', foundational, papal_authority_disposes_non_christian_territory).
narrative_ontology:cs_axiom_status(papal_authority_disposes_non_christian_territory, holdable).
narrative_ontology:cs_axiom_grounding('bca73720-ea9f-4e34-9037-50e52b0a411a', papal_authority_disposes_non_christian_territory, theological).
narrative_ontology:cs_axiom('bca73720-ea9f-4e34-9037-50e52b0a411a', foundational, subjugation_opens_peoples_to_salvation).
narrative_ontology:cs_axiom_status(subjugation_opens_peoples_to_salvation, holdable).
narrative_ontology:cs_axiom_grounding('bca73720-ea9f-4e34-9037-50e52b0a411a', subjugation_opens_peoples_to_salvation, instrumental).
narrative_ontology:cs_axiom('bca73720-ea9f-4e34-9037-50e52b0a411a', secondary, formal_proclamation_constitutes_consent).
narrative_ontology:cs_axiom_status(formal_proclamation_constitutes_consent, holdable).
narrative_ontology:cs_axiom_grounding('bca73720-ea9f-4e34-9037-50e52b0a411a', formal_proclamation_constitutes_consent, conventional).
narrative_ontology:cs_reference_frame('bca73720-ea9f-4e34-9037-50e52b0a411a', papal_plenitude_donation_title).
narrative_ontology:cs_drift_state('bca73720-ea9f-4e34-9037-50e52b0a411a', valladolid_ordinances_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bca73720-ea9f-4e34-9037-50e52b0a411a', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_and_council_of_indies).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos_and_settlers).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, mendicant_orders_and_colonial_church).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, antillean_taino_communities).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, mesoamerican_altepetl_communities).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, andean_ayllu_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacicazgo_kuraka_intermediaries).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacicazgo_kuraka_intermediaries).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_plenitude_of_power_doctrine).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, just_war_infidel_subjugation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the papal-derived license as capitulaciones sold to private commanders, fixes the royal share of all takings, adjudicates between conquerors, settlers, and churchmen through the Council of the Indies, and periodically rewrites the terms — tightening control after the New Laws, retiring private conquest in the 1573 ordinances while keeping the tributary and mining apparatus intact. Revenue flows to a treasury that underwrites European wars; the crown can revoke grants, redirect fleets, and move to new fronts.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_and_council_of_indies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_and_council_of_indies, beneficiary).

% Receive grants assigning them the labor and tribute of specified indigenous communities in return for nominal duties of defense and religious instruction. They command the armed households that actually execute conquest and collection, ship silver and produce home or reinvest in new expeditions, and when a front closes they move to the next one; many return to Spain enriched.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos_and_settlers, beneficiary,
    organized, biographical, mobile, continental).

% Run the doctrinas, baptisms, schools, and parish tithes that the conversion mandate places in their hands, gaining institutional reach across the occupied territories. The same mandate draws a vocal current of their own members into open attack on the conquest's legitimacy — preaching against settler abuses, litigating the title question at court — while the institution as a whole continues to expand on the grant's foundation.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, mendicant_orders_and_colonial_church, beneficiary,
    institutional, generational, identity_locked, continental).

% First-contact communities of the Antilles bear the earliest labor drafts, food requisitions, and forced resettlements; epidemic collapse compounds the exactions. Flight to other islands or into the interior is hunted; the 1511–1515 risings are broken with exemplary executions; no external power acknowledges their jurisdiction.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, antillean_taino_communities, payer,
    powerless, immediate, trapped, regional).

% Organized city-states and lordships confront invasion with large armies and active diplomacy, some allying with the newcomers against dominant neighbors. After defeat they pay assessed tribute, surrender land and labor rotations, endure congregación resettlement into compact towns, and see surviving nobles folded into a co-opted stratum; local governance continues under heavier exactions.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, mesoamerican_altepetl_communities, payer,
    organized, generational, constrained, regional).

% Highland communities are enrolled through co-opted kuraka intermediaries into successive labor regimes — encomienda service, then the state draft rotating workers to the Huancavelica mercury mines and the Potosí silver hill. Uprisings flare repeatedly; the Neo-Inca state at Vilcabamba fights on until 1572. Communal landholding survives, hollowed by mortality and levy quotas.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, andean_ayllu_communities, payer,
    organized, generational, trapped, regional).

% Indigenous lords who retain noble rank, tribute exemptions, and municipal office in exchange for delivering their own communities' quotas of labor, tribute, and converts. Their standing depends on the arrangement they administer downward; those who refuse are replaced, those who comply are resented from below.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacicazgo_kuraka_intermediaries, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, cacicazgo_kuraka_intermediaries, beneficiary).

% France, England, and the Dutch Republic never accept the line or the grant behind it, treating the ocean as open and seizing shipping and trading posts wherever profitable. They sit outside every council that produced or administered the arrangement and owe it nothing.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, atlantic_rival_maritime_powers, excluded,
    powerful, biographical, arbitrage, global).

% Scholars of Salamanca and preachers of Hispaniola argue from canon and natural law that the pope holds no temporal title over infidel polities and that the wars fail every just-war test; they carry the argument to court, effectively win the Valladolid exchange, and see conquests paused — without ever holding a lever that stops the machinery.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dominican_scholastic_critics, observer,
    institutional, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos_and_settlers).
narrative_ontology:fixing_cost_class(tordesillas_demarcation_kernel__spanish_conquest_legitimation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates newly encountered oceanic space between the two Catholic crowns without war between them, and supplies a common legal script — license, capitulación, requirimiento — that turns private armed ventures into royal enterprises with known shares and duties.
% TRANSFER_FUNCTION: Moves land, labor rotation, tribute grain and cloth, silver and mercury output, and formal religious allegiance from the polities and communities west of the line to the Castilian treasury, settler households, and church institutions; moves the costs of disease, conscription, and resettlement onto those same communities.
% ABSENT_VOICES: No indigenous polity sat in any council that drafted the donation, the treaty, the Laws of Burgos, or the New Laws; their objections arrive only as petitions relayed by sympathetic friars or as rebellion. The Atlantic rivals are excluded by construction — the allocation was made between two crowns over space others also used.
% DISAPPEARANCE_RATIONALE: Without the license, conquest expeditions lose lawful warrant and financing, encomienda titles lapse into naked usurpation requiring renegotiation with thousands of armed grantees, the church's conversion mandate loses its juridical frame, and the crown's American revenue loses its legal basis. Occupation would persist by force for a time, but the entire edifice of councils, audits, titles, and tithes built on the grant would have to be rebuilt from nothing.
% FOUNDING_PROBLEM: Two urgent problems circa 1492: prevent Castile and Portugal from going to war over overlapping discoveries, and supply Christian princes a lawful answer to the question of what right permits expansion into non-Christian space at all.
% FOUNDING_PROBLEM_CORROBORATION: Attested dead from outside the beneficiary set: the Salamanca school's public denial that papal temporal title could ground conquest (Vitoria's Relectio de Indis, 1539), the uninterrupted refusal of France, England, and the Dutch Republic to treat the line as binding, and archived indigenous cabildo petitions contesting the exactions the license authorized. Only the grant's own beneficiaries — the crown, the settler class, the colonial church — attest that the founding warrant remained live, and they ceased needing it in practice once possession was armed.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Scores are authored independently of the claim. Extractiveness 0.86: the arrangement's operation transferred land, rotating labor, tribute, and the century's richest mineral flows from the governed population to settler households, the treasury, and the church — even the tradition's own defenders counted the transfer as occurring, disputing only its wrongfulness; the referent is the standing licensed arrangement, assessed by this reading's own lights, in which the transfer is real and enormous. Suppression 0.85 is authored as a raw structural property (unscaled by the engine): conquest arms, garrison towns, resettlement policy that deliberately broke communal ties, and a colonial inquisition; suppression here is overwhelmingly structural, with a thinner internalized layer laid down by doctrina schooling — noted for completeness, not scored separately. Theater_ratio 0.55: early enforcement was brutally functional, but the justification apparatus thickened across the interval — the Requerimiento read aloud to audiences who could not parse it, just-war formalities, and finally the 1573 relabeling of 'conquest' as 'pacification' while the mines kept running — crossing the Goodhart threshold late in the interval, which the measurement series shows rising monotonically. Accessibility_collapse 0.68: for the governed, alternatives (flight, appeal, negotiation) collapsed almost completely; at system level the constraint never achieved closure — rival powers simply declined to be bound — so collapse is deep but incomplete. Resistance 0.74: continuous from Antillean risings through the Arauco and Pueblo wars, plus the intra-Christian juridical assault on the title itself. Coalition potential among the powerless was tested and defeated: pan-island Taíno coordination was broken by divide-and-resettle tactics, which is itself evidence of how much coalition threat the enforcement machinery priced in. The measurement grid is one shared lattice (nine points, three metrics, all authored at every point); trajectories are monotonic ratchets rather than cycles, so no intermittent-reinforcement dynamic is claimed. Rising base_extractiveness across the interval is the accumulation signature the T17 trigger watches; it is reported as measured history, not tuned toward any verdict.
 *
 * PERSPECTIVAL GAP:
 *   Seats must compute apart. From the crown's chair the same structure reads as lawful administration the crown writes, revises, and retires on schedule; from an encomendero's chair, as opportunity with duties attached; from a Taíno or ayllu chair, as annihilation wearing a legal gown; from the mendicant bench, as a mission whose funding mechanism indicts itself; from the Salamanca lecture hall, as a title that fails its own just-war tests. Identity-lock binds the two church seats: their professional and ideological selves are constituted by the evangelization vocation the grant funds, so exit is unthinkable without dissolving the vocation — break that frame and the mendicant seat flips from locked beneficiary to open critic with nothing left to protect, and the scholastic seat loses its last institutional tether. The engine derives these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries anchor the low end: the crown sets and collects (royal fifth, later the mining fiscal share), encomenderos receive labor and tribute directly, the church receives converts, tithes, and reach — d sits near zero for all three, with the church's internal critic wing pulling its effective position somewhat upward. Declared victims anchor the high end: Taíno, altepetl, and ayllu communities bear labor, tribute, land loss, and demographic collapse, with trapped or constrained exits pushing their d toward the full-target pole; co-opted caciques and kurakas occupy a genuinely intermediate position — paying upward while privileged collectors downward — which their dual role encodes. The single authored override corrects the one seat the derivation cannot see: the Atlantic rivals are the only powerful-atom agents in the story, and an uncorrected power-atom fallback could read their exclusion as targetship; their true relation to the transfer is zero — they stand wholly outside it — so d is pinned at 0.5. Overrides keyed by power atom cannot isolate the critic wing inside the institutional church seats; that residual is left to the derivation and flagged here. Spatial scope is global, which the engine's modifier will amplify for targets: verification of what happened beyond the line was weakest precisely where extraction ran hardest.
 *
 * MANDATROPHY ANALYSIS:
 *   Two misclassifications are guarded against. First, the pure-rope temptation: inter-crown peace and a common expansion script were real coordination goods, but they are minor beside the transfer they rode on — naming beneficiaries and victims together forces the hybrid arithmetic rather than letting the peace dividend launder the extraction. Second, the paper-over-force temptation: if the license were mere ceremony atop inevitable violence, the story would decay toward inert formality; the counterfactual omega holds that question open on evidence rather than assumption. On the genealogy battery the arrangement is a textbook zombie: its founding problems (averting Iberian war; supplying a lawful form for first expansion) were dead within a generation, yet the machinery they licensed persisted and deepened — the status-dead x world-rearranges mismatch flags capture, cross-checked here against a theater_ratio that crosses 0.5 exactly as the founding functions lapse. The 1573 ordinances are the mandatrophy event visible in the record: the crown retired the word 'conquest,' not the mine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_indexicality_of_grant,
    'This file instantiates one reading (spanish_conquest_legitimation) of the tordesillas_demarcation_kernel; how would the sibling reading (portuguese_exploration_legitimation) change the constraint''s structure?',
    'Generate the sibling as its own story over the same bulls and treaty line: its referent is exploration and exclusion rights east of the line, its actor set is trading-post commerce, and its victim set is nearly empty.',
    'The sibling should compute far lower epsilon and a rope-or-mild-tangled-rope profile; if instead it computes as a snare over the same text, the kernel decomposes differently than assumed and this file''s victim set is mis-scoped. The disagreement is located in what the papal grant confers: title to dominion over non-Christian peoples versus navigational-commercial exclusivity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_indexicality_of_grant, conceptual, 'Committer structure: this constraint is one reading of a contested kernel; the sibling reading is a different constraint, not a rival opinion about this one.').

omega_variable(
    papal_temporal_title_validity,
    'Did the papal donation ever constitute valid title to temporal dominion over non-Christian polities?',
    'Juridical-historical adjudication of the canon-law question the Salamanca school posed: does plenitudo potentiae extend to disposing of infidel jurisdictions? Trace whether any later legal order ever rested on an affirmative answer.',
    'If invalid ab initio, every downstream title (viceroyalties, audiencias, encomiendas) rests on a null foundation; classification of extraction is unchanged — the behavior is what it is — but the legitimation layer reads as pure assertion rather than defective law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_temporal_title_validity, conceptual, 'Whether the license''s legal foundation was ever sound, independent of how it operated.').

omega_variable(
    license_vs_force_counterfactual,
    'Did the papal-treaty license causally enable the scale of conquest and subjugation, or would epidemiological and military asymmetry have produced comparable dispossession under any banner?',
    'Compare contemporaneous expansions mounted without any papal warrant (northern European colonies) for extraction intensity and victim-set breadth, controlling for disease mortality and military technology.',
    'If unlicensed ventures match the licensed ones, the paper is closer to a formality stretched over force and the snare''s operative mechanism is the settler-military complex rather than the grant; if licensed ventures extract more deeply and durably, the license is load-bearing and the snare classification attaches to it directly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(license_vs_force_counterfactual, empirical, 'Counterfactual weight of the juridical license itself in producing the extraction.').

omega_variable(
    mission_function_separability,
    'How much of the arrangement''s evangelization activity was a genuine service function separable from conquest, and how much was the coordination cover under which extraction ran?',
    'Compare outcomes in zones evangelized outside the conquest-license frame with zones inside it: literacy, mortality, labor burden, and conversion durability.',
    'If the pastoral function performs comparably without the coercive frame, the license''s contribution to evangelization is separable and the excess extraction attributable to it grows; if mission outcomes depend on the frame, part of the measured extraction is a price the reading itself counts as salvation-cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mission_function_separability, empirical, 'Separability of the genuine coordination functions (evangelization, inter-crown peace) from the extraction they sheltered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1493, 1573).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tord_tr_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1493, 0.18).
narrative_ontology:measurement(tord_tr_t1503, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1503, 0.22).
narrative_ontology:measurement(tord_tr_t1513, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1513, 0.34).
narrative_ontology:measurement(tord_tr_t1523, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1523, 0.36).
narrative_ontology:measurement(tord_tr_t1533, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1533, 0.38).
narrative_ontology:measurement(tord_tr_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1542, 0.44).
narrative_ontology:measurement(tord_tr_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1550, 0.47).
narrative_ontology:measurement(tord_tr_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1560, 0.5).
narrative_ontology:measurement(tord_tr_t1573, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1573, 0.55).

% Extraction over time
narrative_ontology:measurement(tord_be_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1493, 0.62).
narrative_ontology:measurement(tord_be_t1503, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1503, 0.68).
narrative_ontology:measurement(tord_be_t1513, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1513, 0.71).
narrative_ontology:measurement(tord_be_t1523, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1523, 0.78).
narrative_ontology:measurement(tord_be_t1533, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1533, 0.83).
narrative_ontology:measurement(tord_be_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1542, 0.81).
narrative_ontology:measurement(tord_be_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1550, 0.83).
narrative_ontology:measurement(tord_be_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1560, 0.85).
narrative_ontology:measurement(tord_be_t1573, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1573, 0.86).

% Suppression requirement over time
narrative_ontology:measurement(tord_su_t1493, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1493, 0.55).
narrative_ontology:measurement(tord_su_t1503, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1503, 0.62).
narrative_ontology:measurement(tord_su_t1513, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1513, 0.66).
narrative_ontology:measurement(tord_su_t1523, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1523, 0.74).
narrative_ontology:measurement(tord_su_t1533, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1533, 0.8).
narrative_ontology:measurement(tord_su_t1542, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1542, 0.79).
narrative_ontology:measurement(tord_su_t1550, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1550, 0.81).
narrative_ontology:measurement(tord_su_t1560, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1560, 0.83).
narrative_ontology:measurement(tord_su_t1573, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1573, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resource_allocation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_exploration_legitimation).

% DUAL FORMULATION NOTE:
% The colloquial label 'Tordesillas' conflates two structurally distinct constraints sharing one kernel text. This file instantiates the western reading (title to conquer and subjugate; high epsilon; victim set west of the line). The sibling file instantiates the eastern reading (confirmation of prior exploration rights and rival exclusion; trading-post actor set; minimal indigenous victimization; far lower epsilon). Epsilon differs because the two readings confer different things over the same bulls and treaty line — reading-indexed epsilon over a shared referent, per the epsilon-invariance principle; the stories are linked here as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, powerful, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
