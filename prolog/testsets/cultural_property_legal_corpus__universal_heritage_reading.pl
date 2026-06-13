% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Reading of Cultural Property Legal Corpus
 *   domain: international_law/post_colonial_studies
 *
 * SUMMARY:
 *   The universal heritage reading frames cultural artifacts as belonging to
 *   humanity and legitimates the authority of Western holding institutions to
 *   retain, preserve, and provide access to artifacts originating outside the
 *   West. This reading emerged in the mid-20th century when newly independent
 *   nations sought repatriation of colonially expropriated material, and
 *   holding institutions responded with a claim that universal preservation
 *   and access served humanity better than return to origin states allegedly
 *   lacking conservation capacity. The reading is instantiated in UNESCO
 *   guidelines, international law standards (UNIDROIT), and the operating
 *   frameworks of major museums and universities. It treats successor states'
 *   repatriation claims as particularist threats to the universal good, and
 *   frames origin communities as beneficiaries of access rather than
 *   custodians. The constraint exhibits rising extractiveness over the
 *   interval (1970–2024) — as claimant states developed institutional
 *   capacity, the standards for repatriation were tightened rather than
 *   relaxed, indicating the technical problem (preservation capacity) was
 *   weaponized as a permanent retention mechanism. Theater ratio rises as
 *   digitization and virtual access rhetoric expand while repatriation rates
 *   remain low, suggesting performative accommodation replacing structural
 *   change.
 *
 * KEY AGENTS:
 *   - major_holding_institutions (agenda_setter/beneficiary): Western museums and universities setting repatriation standards and retaining artifacts under the universal heritage framing
 *   - claimant_successor_states (payer): Nations with cultural continuity to artifact-origin communities, bearing legal and diplomatic costs of repatriation claims
 *   - origin_communities (payer/excluded): Indigenous and traditional communities experiencing identity harm and spiritual loss, lacking standing in international repatriation law
 *   - international_legal_bodies (agenda_setter/beneficiary): UNESCO, UNIDROIT, and academic institutions authoring the consensus framework that legitimates holding-institution authority
 *   - conservation_science_discipline (beneficiary): Professional discipline concentrated in Western institutions, benefiting from the framing that Western expertise is uniquely capable
 *   - alternative_repatriation_advocates (excluded): Indigenous rights organizations and postcolonial scholars advocating for origin-community or successor-state stewardship, systematically excluded from standards-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.78).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.71).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Reading of Cultural Property Legal Corpus").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, '2f641f48-c409-4fce-b70a-64069014b41c').
narrative_ontology:cs_kernel_codification('2f641f48-c409-4fce-b70a-64069014b41c', fixed_text).
narrative_ontology:cs_authority_grounding('2f641f48-c409-4fce-b70a-64069014b41c', extraction).
narrative_ontology:cs_interpretation_layer_present('2f641f48-c409-4fce-b70a-64069014b41c').
narrative_ontology:cs_reading_relation('2f641f48-c409-4fce-b70a-64069014b41c', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2f641f48-c409-4fce-b70a-64069014b41c', cultural_property_legal_corpus__indigenous_stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('2f641f48-c409-4fce-b70a-64069014b41c', foundational, universal_stewardship_authority).
narrative_ontology:cs_axiom_status(universal_stewardship_authority, holdable).
narrative_ontology:cs_axiom_grounding('2f641f48-c409-4fce-b70a-64069014b41c', universal_stewardship_authority, instrumental).
narrative_ontology:cs_axiom('2f641f48-c409-4fce-b70a-64069014b41c', foundational, preservation_capacity_prerequisites).
narrative_ontology:cs_axiom_status(preservation_capacity_prerequisites, holdable).
narrative_ontology:cs_axiom_grounding('2f641f48-c409-4fce-b70a-64069014b41c', preservation_capacity_prerequisites, empirically_contingent).
narrative_ontology:cs_reference_frame('2f641f48-c409-4fce-b70a-64069014b41c', universal_preservation_mandate).
narrative_ontology:cs_drift_state('2f641f48-c409-4fce-b70a-64069014b41c', contemporary_post_decolonial_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f641f48-c409-4fce-b70a-64069014b41c', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, wealthy_diaspora_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, claimant_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, origin_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_legal_bodies).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, conservation_science_discipline).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Western museums and universities hold the majority of culturally significant artifacts originating outside the West. Under the universal heritage reading, they set repatriation policy, determine who qualifies as a legitimate claimant, and define preservation standards. They frame retention as serving humanity and claim authority based on conservation expertise and accessibility infrastructure. Direct beneficiaries of prestige, funding, and research access tied to artifact collections.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Nations with historical and legal continuity to communities whose artifacts were colonially expropriated must navigate an international legal framework requiring proof of ownership, cultural continuity, and institutional capacity — all standards written by holding institutions. Repatriation requests are costly (legal fees, documentation, expert reports), diplomatically draining (treated as threats to universal access), and frequently denied. Exit is impossible without abandoning sovereignty claims.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, claimant_successor_states, payer,
    powerful, generational, constrained, global).

% Indigenous and traditional communities whose sacred or culturally central artifacts remain in distant institutions experience ongoing estrangement from material heritage. Lack standing in international law (only successor states petition). Their relationship to artifacts is redefined as access rather than custodianship. Experience identity harm and spiritual loss; exit would require abandoning identity claims to the artifacts.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, origin_communities, payer,
    powerless, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, origin_communities, excluded).

% UNESCO, UNIDROIT, and academic institutions produce the consensus framework legitimating the universal reading. They author repatriation standards, participate in advisory committees, and define what counts as adequate proof of continuity and capacity. Maintain authority to adjudicate disputes. Frame role as neutral stewardship while producing standards that align with holding-institution interests.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_legal_bodies, agenda_setter,
    institutional, generational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, international_legal_bodies, beneficiary).

% Professional discipline concentrated in Western holding institutions. Benefits from exclusive access to comprehensive collections, research collaboration networks centered on holding institutions, and prestige tied to global artifact work. Expertise mobilized to justify universal reading's claim that Western institutions are uniquely capable custodians.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, conservation_science_discipline, beneficiary,
    organized, biographical, mobile, global).

% Diaspora groups from artifact-origin regions access heritage through Western museums' exhibitions, publications, and research. Benefit from institutional preservation and scholarly infrastructure. Cosmopolitan position allows framing repatriation in ways aligned with holding-institution interests. Sufficient exit optionality to leave constraint without harm.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, wealthy_diaspora_communities, beneficiary,
    organized, biographical, mobile, global).

% Indigenous rights organizations, postcolonial scholars, and repatriation-advocacy networks argue stewardship should rest with origin communities or successor states, not holding institutions. Systematically excluded from consensus-building processes. Treated as culturally important but politically particularist. Suppressed through rhetorical delegitimation and institutional subordination.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, alternative_repatriation_advocates, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides centralized, professional preservation and scholarly access to culturally significant artifacts that might otherwise be lost to neglect, conflict, environmental damage, or lack of institutional capacity. Solves the coordination problem of ensuring humanity's material heritage survives and is available for research and edification across geographic and cultural boundaries.
% TRANSFER_FUNCTION: Moves authority to define preservation, research, and access from origin communities and successor states to Western holding institutions and international legal bodies. Moves costs of proof (legal, documentary, technical) from holding institutions to claimants. Moves material custody from communities of origin to distant institutions; moves cultural meaning-making from communities to curatorial and scholarly experts.
% ABSENT_VOICES: Indigenous communities whose artifacts are held lack standing in international repatriation processes (only successor states petition); they are consulted but rarely leading. Postcolonial scholars and repatriation advocates argue the universal reading is a particularist political project but are excluded from standards-setting bodies. Origin-country conservation scientists and cultural specialists are rarely represented in international bodies defining preservation adequacy and repatriation conditions.
% DISAPPEARANCE_RATIONALE: If the universal heritage reading and its enforcement machinery disappeared — if the legal default shifted to claimant-state ownership with return as expected outcome — the global distribution of cultural artifacts would shift radically. Major Western museums would face return requests they cannot legally resist; origin states would recover material heritage; conservation science would reorganize around distributed expertise; scholarship would reorient toward collaborative rather than extractive research. The constraint's disappearance would trigger massive institutional reorganization.
% FOUNDING_PROBLEM: In the mid-20th century, as colonies gained independence, significant cultural artifacts remained in Western institutions acquired during colonial rule. Concerns arose that returning artifacts to newly independent states with limited conservation infrastructure would result in loss through neglect, conflict, or environmental damage; the problem was posed as technical rather than political.
% FOUNDING_PROBLEM_CORROBORATION: Holding institutions and conservation-science professionals attest the problem is live and ongoing — developing-world institutions lack resources for world-class conservation. Origin-state governments and indigenous rights organizations attest the founding problem has been substantially weaponized — capacity is used to justify permanent retention, yet many claimant nations built institutional capacity over decades. UNESCO reports document both the real conservation challenges AND the pattern of repatriation requests denied on capacity grounds without systematic reassessment as claimant capabilities improved.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint scores high on extractiveness (0.78 at interval end) because holding institutions directly benefit from retention — prestige, collection completeness, funding tied to rare artifacts, research access, and institutional authority. Suppression is substantial (0.71) because the constraint's persistence depends on actively maintaining the legal and rhetorical barriers that make repatriation legally difficult: capacity standards, continuity proofs, institutional-capacity requirements all written by and for holding institutions. Theater ratio rises from 0.12 (1970) to 0.41 (2024) as the constraint's maintenance increasingly involves performative gestures (digitization projects, collaborative research agreements, repatriation by loan) that substitute for structural return of custody. The coercion grid shows asymmetric pressure: at the structural level, the universal heritage reading is institutionalized with high accessibility collapse (0.85 at t_n) and institutional suppression (0.82); at the individual level (origin community members, claimant-state citizens), the constraint's force is lower (individual-level accessibility collapse 0.42, suppression 0.48) but persistent because structural-level enforcement constrains individual exit options. Resistance rises across all levels (individual 0.35→0.52, organizational 0.48→0.74, class 0.42→0.81, structural 0.55→0.72), indicating growing mobilization against the reading, particularly at the class level (indigenous and postcolonial solidarity movements) and organizational level (successor-state governments, repatriation-advocacy networks).
 *
 * PERSPECTIVAL GAP:
 *   See logic_rationale and directionality_logic sections above — the gap is structural, not observational.
 *
 * DIRECTIONALITY LOGIC:
 *   Holding institutions are structural beneficiaries (d near 0.0): they set rules, retain artifacts, control research access, and benefit from prestige and funding. Their exit optionality is high (arbitrage) — they can simply refuse to participate in the constraint, but refusing means losing the authority to define what happens to the artifacts. Claimant successor states are targets (d near 0.75): they bear legal costs, diplomatic friction, and the cost of proving themselves adequate custodians under standards they did not set. Their exit is constrained (they cannot abandon the claim without abandoning the national narrative of cultural continuity). Origin communities are targets with identity lock (d near 0.85): they experience the constraint as spiritual and cultural loss; their exit would require severing the identity claim to the artifacts, which is impossible. International legal bodies benefit indirectly (d near 0.1-0.2): they maintain authority to adjudicate repatriation disputes and their technical framing (universalism, preservation capacity) aligns with holding-institution interests. Conservation scientists benefit (d near 0.0-0.1): their expertise is mobilized to support the constraint and they have mobile exit options (they can move to other institutions or research areas). Alternative repatriation advocates are payers (d near 0.9): they bear the cost of exclusion from consensus-building and their voice is suppressed rhetorically (delegitimized as emotional or nationalist). Wealthy diaspora benefit (d near 0.2): they can access heritage through museums and have sufficient mobility to exit the constraint without material loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope (not snare) because it carries a genuine coordination function (preservation, access) alongside asymmetric extraction (legal friction, authority captured by beneficiaries). The classification prevents both misreadings: (1) reading it as pure rope (coordination only) would miss the extractiveness; (2) reading it as pure snare would miss the real preservation work that the constraint does accomplish. The constraint exhibits mandatrophy ambiguity because the founding problem (preservation capacity in newly independent states) was real in 1970 but substantially resolved by 2024 — claimant nations built institutional capacity, yet the constraint persisted with rising extractiveness rather than relaxing. The measurement series documents this: base_extractiveness rose as capacity concerns should have declined, indicating the technical problem was weaponized as a retention mechanism. Theater ratio rising (performative accommodation substituting for return) is a mandatrophy signal: the constraint is increasingly maintained through theater rather than through genuine coordination benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preservation_capacity_weaponization,
    'Is the documented rise in claimant-state conservation capacity (1970–2024) reflected in systematic re-evaluation of repatriation requests, or does the capacity-standard bar rise as capacity improves?',
    'Systematic audit of repatriation decisions against time-stamped assessments of claimant-state institutional capacity; comparison of decision rationales from 1970–1990 (capacity-limited) vs. 2010–2024 (capacity-improved).',
    'If the bar rises with capacity (technical problem was weaponized), the constraint is pure extraction riding on a genuine coordination function that has been substantially accomplished; if the bar falls with improving capacity, the constraint is genuine tangled rope with both functions intact. A weaponized bar would support higher effective extraction and support mandatrophy diagnosis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preservation_capacity_weaponization, empirical, 'Whether preservation-capacity standards track actual capacity changes or serve as permanent retention mechanism.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Can the universal heritage reading and the indigenous-stewardship reading coexist in the same legal and ethical framework, or does adoption of one reading logically foreclose the other?',
    'Doctrinal analysis of whether universal stewardship (maximum preservation/access) and indigenous communal custodianship (sacred/restricted access) can be simultaneously held within one institution''s governing mandate; case-study examination of institutions attempting both.',
    'If foreclosing (one rules out the other), the readings are in genuine logical conflict and the coexist_with relation should be forecloses. If coexisting (both can be institutionalized simultaneously), the three readings truly coexist as alternative authority framings. Foreclosure would indicate the readings are deeper ideological commitments than mere policy alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether the three kernel readings are logically compatible or mutually exclusive.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'For origin communities (identity_locked exit), is the suppression of repatriation claims primarily structural (legal barriers, institutional power differentials, exclusion from decision-making) or internalized (communities have absorbed the framing that universal access is better than return)?',
    'Post-exclusion trajectory analysis: if structural suppression dominates, communities gain voice and advocacy momentum when structural barriers fall; if internalized, suppression persists even as formal barriers relax. Comparative analysis of communities where structural exclusion has been partially lifted (e.g., through national legislation in origin countries).',
    'If structural, reducing legal barriers and increasing voice in repatriation processes would shift the constraint substantially toward origin-community benefit. If internalized, the constraint persists through community internalization of holding-institution framing even if structural barriers fall — higher effective suppression than measurements capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of origin-community claims is structural exclusion or internalized deference to holding-institution authority.').

omega_variable(
    universal_reading_beneficiary_divergence,
    'Which stakeholder group (if any) actually benefits from the universal heritage reading as distinct from holding institutions: are wealthy diaspora genuinely beneficiaries (access they value), or do they benefit only instrumentally as allies of holding institutions?',
    'Stakeholder interviews and preference elicitation: do wealthy diaspora prioritize universal museum access to their heritage, or do they support repatriation to origin communities even at cost to global access? Comparative analysis of diaspora positions in contexts where return has been negotiated.',
    'If diaspora beneficiaries are distinct from institutional allies, the constraint''s beneficiary set is genuinely distributed and the asymmetry is more moderate. If diaspora support returns when given voice, they are misclassified as beneficiaries and the extraction is more concentrated than the two-beneficiary structure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_reading_beneficiary_divergence, empirical, 'Whether wealthy diaspora communities are actual beneficiaries or strategic allies of holding institutions.').

omega_variable(
    reading_sibling_foreclosure_determination,
    'Does the universal heritage reading''s core axiom (legitimate authority rests with institutions maximizing preservation/access regardless of origin) logically foreclose the indigenous-stewardship reading''s core axiom (legitimate authority rests with communities maintaining cultural continuity), or do these remain compatible premises held by different parties?',
    'Logical analysis of whether maximum universal access is structurally incompatible with restricted community-stewardship access; examination of whether a hybrid framework (universal access + community custody + sacred-access restrictions) is coherent or self-contradictory.',
    'Determination of reading_relations: if foreclosing, relation should be forecloses; if compatible, relation should be coexists_with. This affects how the engine models the kernel contest — is it a dispute over facts or values, or a conflict of incompatible authority claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_sibling_foreclosure_determination, conceptual, 'Logical compatibility of the universal and indigenous-stewardship readings'' core premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t1970, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(cult_tr_t1970, observed).
narrative_ontology:measurement(cult_tr_t1985, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement_basis(cult_tr_t1985, observed).
narrative_ontology:measurement(cult_tr_t2000, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(cult_tr_t2000, observed).
narrative_ontology:measurement(cult_tr_t2012, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2012, 0.36).
narrative_ontology:measurement_basis(cult_tr_t2012, observed).
narrative_ontology:measurement(cult_tr_t2018, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2018, 0.39).
narrative_ontology:measurement_basis(cult_tr_t2018, observed).
narrative_ontology:measurement(cult_tr_t2024, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 2024, 0.41).
narrative_ontology:measurement_basis(cult_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t1970, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement_basis(cult_be_t1970, observed).
narrative_ontology:measurement(cult_be_t1985, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 1985, 0.54).
narrative_ontology:measurement_basis(cult_be_t1985, observed).
narrative_ontology:measurement(cult_be_t2000, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement_basis(cult_be_t2000, observed).
narrative_ontology:measurement(cult_be_t2012, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2012, 0.72).
narrative_ontology:measurement_basis(cult_be_t2012, observed).
narrative_ontology:measurement(cult_be_t2018, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2018, 0.76).
narrative_ontology:measurement_basis(cult_be_t2018, observed).
narrative_ontology:measurement(cult_be_t2024, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(cult_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t1970, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement_basis(cult_su_t1970, observed).
narrative_ontology:measurement(cult_su_t1985, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 1985, 0.54).
narrative_ontology:measurement_basis(cult_su_t1985, observed).
narrative_ontology:measurement(cult_su_t2000, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(cult_su_t2000, observed).
narrative_ontology:measurement(cult_su_t2012, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2012, 0.68).
narrative_ontology:measurement_basis(cult_su_t2012, observed).
narrative_ontology:measurement(cult_su_t2018, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement_basis(cult_su_t2018, observed).
narrative_ontology:measurement(cult_su_t2024, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 2024, 0.71).
narrative_ontology:measurement_basis(cult_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2024
narrative_ontology:measurement(cult_grid_01, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(class), 1970, 0.42).
narrative_ontology:measurement(cult_grid_02, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(class), 2024, 0.52).
narrative_ontology:measurement(cult_grid_03, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(individual), 1970, 0.35).
narrative_ontology:measurement(cult_grid_04, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(individual), 2024, 0.42).
narrative_ontology:measurement(cult_grid_05, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(organizational), 1970, 0.58).
narrative_ontology:measurement(cult_grid_06, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(organizational), 2024, 0.68).
narrative_ontology:measurement(cult_grid_07, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(structural), 1970, 0.7).
narrative_ontology:measurement(cult_grid_08, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(structural), 2024, 0.85).
narrative_ontology:measurement(cult_grid_09, cultural_property_legal_corpus__universal_heritage_reading, resistance(class), 1970, 0.42).
narrative_ontology:measurement(cult_grid_10, cultural_property_legal_corpus__universal_heritage_reading, resistance(class), 2024, 0.81).
narrative_ontology:measurement(cult_grid_11, cultural_property_legal_corpus__universal_heritage_reading, resistance(individual), 1970, 0.35).
narrative_ontology:measurement(cult_grid_12, cultural_property_legal_corpus__universal_heritage_reading, resistance(individual), 2024, 0.52).
narrative_ontology:measurement(cult_grid_13, cultural_property_legal_corpus__universal_heritage_reading, resistance(organizational), 1970, 0.48).
narrative_ontology:measurement(cult_grid_14, cultural_property_legal_corpus__universal_heritage_reading, resistance(organizational), 2024, 0.74).
narrative_ontology:measurement(cult_grid_15, cultural_property_legal_corpus__universal_heritage_reading, resistance(structural), 1970, 0.55).
narrative_ontology:measurement(cult_grid_16, cultural_property_legal_corpus__universal_heritage_reading, resistance(structural), 2024, 0.72).
narrative_ontology:measurement(cult_grid_17, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(class), 1970, 0.38).
narrative_ontology:measurement(cult_grid_18, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(class), 2024, 0.64).
narrative_ontology:measurement(cult_grid_19, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(individual), 1970, 0.22).
narrative_ontology:measurement(cult_grid_20, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(individual), 2024, 0.38).
narrative_ontology:measurement(cult_grid_21, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(organizational), 1970, 0.45).
narrative_ontology:measurement(cult_grid_22, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(organizational), 2024, 0.71).
narrative_ontology:measurement(cult_grid_23, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(structural), 1970, 0.52).
narrative_ontology:measurement(cult_grid_24, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(structural), 2024, 0.79).
narrative_ontology:measurement(cult_grid_25, cultural_property_legal_corpus__universal_heritage_reading, suppression(class), 1970, 0.45).
narrative_ontology:measurement(cult_grid_26, cultural_property_legal_corpus__universal_heritage_reading, suppression(class), 2024, 0.68).
narrative_ontology:measurement(cult_grid_27, cultural_property_legal_corpus__universal_heritage_reading, suppression(individual), 1970, 0.32).
narrative_ontology:measurement(cult_grid_28, cultural_property_legal_corpus__universal_heritage_reading, suppression(individual), 2024, 0.48).
narrative_ontology:measurement(cult_grid_29, cultural_property_legal_corpus__universal_heritage_reading, suppression(organizational), 1970, 0.58).
narrative_ontology:measurement(cult_grid_30, cultural_property_legal_corpus__universal_heritage_reading, suppression(organizational), 2024, 0.82).
narrative_ontology:measurement(cult_grid_31, cultural_property_legal_corpus__universal_heritage_reading, suppression(structural), 1970, 0.62).
narrative_ontology:measurement(cult_grid_32, cultural_property_legal_corpus__universal_heritage_reading, suppression(structural), 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.22).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel cultural_property_legal_corpus. The universal_heritage_reading frames holding institutions as beneficiaries and successor states/origin communities as victims. The sovereign_repatriation_reading frames successor states as beneficiaries and holding institutions as payers. The indigenous_stewardship_reading frames origin communities as beneficiaries and both holding institutions and successor-state governments as constrained by community authority. Each reading has a distinct ε (extraction from different victims), distinct beneficiary/victim structure, and distinct legal/rhetorical mechanisms. The three readings coexist as live positions held by different parties in active dispute; no single framework encompasses all three. The network links capture the doctrinal and institutional dependencies: the universal reading's dominance in international law shapes the structural constraints the repatriation reading must overcome; indigenous-stewardship claims are often reframed as local conservation preferences rather than authority claims under the universal reading's frame. Each reading should be consulted alongside the others to understand the full kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, powerless, 0.88).
constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
