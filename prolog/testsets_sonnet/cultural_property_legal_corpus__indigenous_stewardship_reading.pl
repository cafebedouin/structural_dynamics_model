% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Cultural Property Legal Corpus — Indigenous Stewardship Reading
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   Thousands of sacred and ceremonial objects — held in encyclopedic museums
 *   or repatriated to successor states under bilateral agreements — remain
 *   outside the custody of the indigenous communities that made them, use
 *   them ceremonially, and maintain unbroken cultural transmission around
 *   them. The existing legal machinery (national patrimony statutes,
 *   UNESCO/UNIDROIT frameworks, museum deaccession policy) recognizes only
 *   states and institutions as parties capable of holding or transferring
 *   title. Under the stewardship reading, this is not incidental — it is the
 *   mechanism by which continuity-based claims are structurally excluded from
 *   a corpus of law built around sovereign and institutional title.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.87).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.79).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property Legal Corpus — Indigenous Stewardship Reading").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '41096eed-1253-4ae1-a985-621a1631f7b9').
narrative_ontology:cs_kernel_codification('41096eed-1253-4ae1-a985-621a1631f7b9', distributed).
narrative_ontology:cs_authority_grounding('41096eed-1253-4ae1-a985-621a1631f7b9', distributed).
narrative_ontology:cs_reading_relation('41096eed-1253-4ae1-a985-621a1631f7b9', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('41096eed-1253-4ae1-a985-621a1631f7b9', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_axiom('41096eed-1253-4ae1-a985-621a1631f7b9', foundational, continuity_of_practice_grounds_legitimate_custody).
narrative_ontology:cs_axiom_status(continuity_of_practice_grounds_legitimate_custody, holdable).
narrative_ontology:cs_axiom_grounding('41096eed-1253-4ae1-a985-621a1631f7b9', continuity_of_practice_grounds_legitimate_custody, deontological).
narrative_ontology:cs_axiom('41096eed-1253-4ae1-a985-621a1631f7b9', foundational, statehood_is_not_sufficient_for_cultural_authority).
narrative_ontology:cs_axiom_status(statehood_is_not_sufficient_for_cultural_authority, holdable).
narrative_ontology:cs_axiom_grounding('41096eed-1253-4ae1-a985-621a1631f7b9', statehood_is_not_sufficient_for_cultural_authority, conventional).
narrative_ontology:cs_reference_frame('41096eed-1253-4ae1-a985-621a1631f7b9', continuity_of_practice_authority).
narrative_ontology:cs_drift_state('41096eed-1253-4ae1-a985-621a1631f7b9', post_1990s_repatriation_movement_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('41096eed-1253-4ae1-a985-621a1631f7b9', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities_of_origin).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_diaspora_communities).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_continuity_grounds_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community whose ancestors made, used, and held sacred the artifacts now sits outside the legal chain of custody entirely — national patrimony statutes and museum deaccession policies both route title to states or institutions, never to the community as such. They cannot litigate for return because the applicable law does not recognize them as a legal person capable of holding the claim; their only paths are moral appeal, media campaigns, or negotiating loans of objects they consider theirs to borrow. Cultural continuity — living ceremonial use, oral transmission, unbroken descent from the makers — carries no legal weight against a museum's registered title or a state's constitutional claim to 'national heritage.'
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities_of_origin, payer,
    powerless, civilizational, trapped, regional).

% Displaced or diasporic descendants of the originating community face an even steeper bar: they lack both territorial presence and the state-recognition that even flawed repatriation frameworks require. They watch objects central to their identity displayed as 'world art' or claimed by a successor state with which they may have an adversarial relationship, with no forum in which continuity-of-practice, rather than geography or statehood, is the operative legal test.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_diaspora_communities, payer,
    powerless, generational, trapped, global).

% Holds title through documented (if colonial-era) acquisition, sets its own deaccession policy, and negotiates repatriation only as discretionary goodwill rather than legal obligation. Retains full curatorial and physical control, derives prestige, ticket revenue, and scholarly capital from the collection, and can indefinitely defer any claim that does not originate from a state with treaty leverage. Under this reading, the museum has no legitimate claim at all — its title rests on acquisition inside a colonial extraction system, not on any relationship of continuity to the objects.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, encyclopedic_museums, beneficiary).

% Where the successor state itself was built on the displacement or subordination of the indigenous originating community (settler states, post-colonial states that absorbed multiple distinct peoples into one nationality), it asserts sovereign patrimony over artifacts as 'national heritage' and negotiates repatriation government-to-government or museum-to-state — routing the outcome, and any restitution, through itself rather than to the community. Under this reading it is a second extractor, structurally similar to the museum: it did not make the objects sacred and has no continuity relationship to them, but it captures the legal standing to decide their fate.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_successor_states, beneficiary).

% UNESCO conventions, UNIDROIT, and national patrimony law recognize states as the primary claimants and museums as the primary holders; none currently provide a standing cause of action for a sub-state indigenous community as such. Scholars and rapporteurs increasingly document the gap between community-based continuity claims and the state-centric machinery, but the machinery itself has not been amended to close it.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_law_bodies, observer,
    institutional, generational, analytical, global).

% Auction houses and private collectors are not parties to the repatriation debate at all under any reading, yet their continued participation in a market for artifacts of contested title depends on exactly the ambiguity this constraint measures — as long as legitimate authority is unsettled between museum, state, and community, objects keep moving through markets that none of the three fully controls.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, market_intermediaries, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Notionally, the corpus of cultural property law coordinates preservation, provenance documentation, and orderly transfer of artifacts across borders and institutions, preventing a return to unregulated looting.
% TRANSFER_FUNCTION: Physical custody, exhibition revenue, scholarly authority, and the power to decide an artifact's fate move to and stay with museums and successor states; nothing structurally moves to the originating community, which under this reading is the only party with a legitimate claim.
% ABSENT_VOICES: The originating indigenous communities and their diaspora descendants are the parties with the strongest claim under this reading and have no standing in the actual legal fora (national courts, UNESCO mechanisms, bilateral state negotiations) where restitution is decided — they can petition or protest but cannot bring the claim as a matter of right.
% DISAPPEARANCE_RATIONALE: If the current state-and-museum-centric legal architecture disappeared and were replaced by one that recognized cultural-continuity-based community standing, custody, exhibition rights, and revenue from thousands of contested collections would shift from institutions and successor states toward maintaining communities; museums would lose title to core holdings and states would lose their monopoly on negotiating restitution on 'the nation's' behalf.
% FOUNDING_PROBLEM: The corpus was built to regularize the disorderly, often violent movement of cultural artifacts after colonial conquest and looting — to replace outright plunder with documented acquisition, and later to give newly independent states a mechanism to reclaim 'national' patrimony from former colonial powers.
% FOUNDING_PROBLEM_CORROBORATION: Museum professional associations and successor-state governments attest the framework functions well, citing successful state-to-state repatriations as evidence of a solved problem. Indigenous rights scholars, UN Special Rapporteurs on the rights of indigenous peoples, and community-led restitution movements (attesting from outside both the museum and successor-state benefiting parties) report that the framework's state-centric design routes restitution past the communities with the actual continuity claim, corroborating that the founding problem — legitimate custody following the community that maintains the culture — remains substantially unsolved even where state-to-state restitution succeeds.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.87) because under this reading essentially every current holder — museum or successor state alike — lacks the legitimating relationship (continuity of practice, descent, ceremonial use) that the reading holds is the actual basis of authority; both categories of holder derive benefit (prestige, revenue, sovereign legitimacy-by-patrimony) from custody that this reading regards as without foundation. Suppression is high (0.79) because the exclusion is doctrinal, not merely practical — indigenous communities as such have no standing in the fora where custody is adjudicated, and this has hardened rather than relaxed even as state-to-state repatriation has become more common (the suppression_requirement series rises because building out state-centric repatriation frameworks has, under this reading, entrenched rather than dissolved the community's exclusion). Theater ratio rises moderately (0.20→0.42) reflecting the growing gap between prominent, publicized state-to-state repatriations and the underlying absence of any mechanism giving communities standing.
 *
 * PERSPECTIVAL GAP:
 *   From the museum's and successor state's seats, the corpus is a functioning (if imperfect) coordination mechanism increasingly correcting colonial-era wrongs through documented restitution. From the originating community's seat, the same corpus is a closed system that reallocates custody between two illegitimate claimants (institution and state) without ever routing authority to the party this reading holds is the only legitimate one. The engine computes these as different seat-level classifications from the same structural facts — the divergence is exactly what this reading is built to expose.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities of origin and diaspora descendants are declared victims: they bear the cost of exclusion from standing, and under this reading they are the parties from whom legitimate authority is being withheld, pushing their derived directionality toward the full-target end regardless of their lack of formal legal power. Museums and successor states are declared beneficiaries/extractors: both retain custody, revenue, or sovereign legitimacy through a system this reading holds neither has a valid claim to operate. Market intermediaries are excluded from the beneficiary/victim structure proper — they profit from the unsettled contest between readings rather than from this reading's specific extraction relationship, which is why they are marked excluded rather than beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — regularizing the disorderly transfer of cultural property after colonial plunder — is treated by museums and successor states as substantially solved via documented acquisition and periodic state-to-state restitution. Under the stewardship reading this is a mandatrophy trap: the mechanism built to solve the problem has outlived the framing that made it adequate, because the actual injury (severing artifacts from the communities that maintain their meaning) was never addressed by a framework that only ever recognized institutions and states as parties. Classifying this as a distinct reading, rather than folding it into the sovereign_repatriation_reading's classification, prevents the analytical error of treating 'restitution has occurred' (state-to-state) as evidence against extraction, when under this reading state-to-state restitution can itself be an extractive transfer between two illegitimate claimants.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_test_specification,
    'What operational test would establish that a given community ''maintains cultural continuity'' sufficient to ground a legitimate claim, as opposed to a merely genealogical or geographic connection?',
    'Comparative ethnographic and legal analysis of the (rare) jurisdictions that have implemented continuity-of-practice standing tests for restitution claims, assessing workability and contestation rates.',
    'A workable, widely accepted continuity test would sharpen this reading from a normative claim into an administrable legal standard; the absence of one is part of why the reading currently has no forum, and resolving it materially changes how tractable the reading''s implementation would be.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(continuity_test_specification, conceptual, 'Whether cultural continuity can be operationalized as a legal standing test.').

omega_variable(
    sibling_reading_foreclosure_or_coexistence,
    'Does adopting the indigenous_stewardship_reading logically foreclose the sovereign_repatriation_reading, or can both be held simultaneously by treating the successor state as a trustee obligated to pass custody to the community?',
    'Doctrinal analysis of whether any existing legal framework treats state custody as trust-for-community rather than sovereign title-in-itself; if such trust-structures exist and are legally enforceable, the two readings can coexist through a state-as-trustee bridge rather than one foreclosing the other.',
    'If no enforceable trust structure exists, the sovereign_repatriation_reading''s core premise (state title as legitimate remedy for colonial extraction) and this reading''s core premise (only continuity-communities are legitimate) remain in tension at the level of who exercises authority day-to-day, even though this document declares the relation as coexists_with at the level of party practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_or_coexistence, conceptual, 'Whether state-as-trustee framings reconcile the stewardship and repatriation readings or merely paper over a live contradiction.').

omega_variable(
    successor_state_indigenous_identity_overlap,
    'In cases where the successor state''s population substantially IS the descendant community (e.g., a post-colonial state whose citizenry largely descends from the originating culture), does the beneficiary/victim split between ''colonial_successor_states'' and ''indigenous_communities_of_origin'' still hold, or does it collapse?',
    'Case-by-case analysis distinguishing settler-successor states (population largely displaced or supplanted the originating community) from post-colonial states with high demographic continuity between citizenry and originating community.',
    'Where continuity is high, the state may function as a reasonable proxy for the community and the extraction/victim split weakens; where continuity is low (settler states, multi-ethnic states absorbing distinct peoples), the split as authored holds at full strength.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(successor_state_indigenous_identity_overlap, empirical, 'Whether demographic continuity between state and community collapses the state/community beneficiary split in some cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cult_tr_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(cult_tr_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(cult_tr_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 32, 0.4).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(cult_be_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(cult_be_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 16, 0.77).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 24, 0.81).
narrative_ontology:measurement(cult_be_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 32, 0.85).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.87).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(cult_su_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(cult_su_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(cult_su_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, universal_heritage_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the cultural_property_legal_corpus kernel. universal_heritage_reading treats preservation-optimizing institutions as legitimate regardless of origin (lowest ε among the three, since institutional custody is itself the legitimating relationship). sovereign_repatriation_reading treats successor states as legitimate claimants against colonial extraction (mid ε — museums are extractors, states are beneficiaries/victims-turned-remediators). This indigenous_stewardship_reading treats BOTH museums and successor states as illegitimate holders relative to the originating community, producing the highest ε of the three. All three share the same underlying artifact-custody facts; they differ in which party's claim the reading treats as legitimating. Do not average across them — each is a distinct, ε-invariant constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
