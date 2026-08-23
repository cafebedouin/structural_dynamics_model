% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__competence_transmission_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__competence_transmission_reading, []).

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
 *   constraint_id: catastrophe_memory_survival__competence_transmission_reading
 *   human_readable: Ritual Survival-Knowledge Transmission System (Competence Reading)
 *   domain: religious_studies/collective_memory
 *
 * SUMMARY:
 *   This story instantiates the competence_transmission_reading of the
 *   catastrophe_memory_survival kernel: it treats ritual as a
 *   storage-and-rehearsal technology for practical survival knowledge,
 *   planting and harvest timing, rationing and resource-management rules,
 *   kin-obligation protocols that pool risk, and adaptation strategies for
 *   displacement. The standing arrangement under contest is living ritual
 *   practice as a transmission institution, and epsilon is assessed for that
 *   arrangement by this reading's own lights: the genuine service is moving
 *   rare-catastrophe knowledge across generations that cannot learn it by
 *   direct experience, while the extraction is the performance burden borne
 *   by communities whose copy of the content has decayed past intelligibility
 *   and by households funding ceremony at customary scale regardless of
 *   harvest. The reading's characteristic asymmetry: diaspora communities,
 *   who need portable adaptive capacity most, receive the encoded protocols,
 *   while origin communities locked into the performance calendar supply the
 *   labor. The claimed type (tangled_rope) is authored from structural
 *   belief; the metrics are authored independently as descriptive estimates,
 *   and any divergence between them and engine-computed per-seat types is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - diaspora_communities: primary beneficiary (moderate/mobile) — carried the ritual package through displacement and receives its encoded timing, rationing, and adaptation protocols as working adaptive capacity
 *   - content_atrophied_practicing_communities: primary target (moderate/identity_locked) — perform the full calendar in liturgical registers they no longer decode, supplying rehearsal labor and material cost for little decodable return
 *   - cost_burdened_poor_households: secondary target (powerless/constrained) — obligated to fund feasts, offerings, and life-cycle rites at customary scale; ceremonial debt is a recognized channel into poverty
 *   - clerical_transmission_hierarchy: agenda-setter and collector (institutional/identity_locked) — controls decoding, ordains transmitters, sets the calendar, and receives offerings, stipends, and the authority rents of gatekeeping
 *   - secularized_descendants: excluded voice (moderate/mobile) — face the same catastrophe-era risks with no access to the gated protocols; would argue for open publication of the practical content
 *   - disaster_ethnographers: analytical observer (organized/analytical) — test ritual calendars against ecological and geological records; their findings feed both reform and traditionalist defense
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, 0.52).
domain_priors:suppression_score(catastrophe_memory_survival__competence_transmission_reading, 0.58).
domain_priors:theater_ratio(catastrophe_memory_survival__competence_transmission_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_survival__competence_transmission_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__competence_transmission_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_survival__competence_transmission_reading, "Ritual Survival-Knowledge Transmission System (Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_survival__competence_transmission_reading, "religious_studies/collective_memory").

domain_priors:requires_active_enforcement(catastrophe_memory_survival__competence_transmission_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__competence_transmission_reading, '5fe9b716-cefb-4346-98fc-8321820c4399').
narrative_ontology:cs_kernel_codification('5fe9b716-cefb-4346-98fc-8321820c4399', distributed).
narrative_ontology:cs_authority_grounding('5fe9b716-cefb-4346-98fc-8321820c4399', lineage).
narrative_ontology:cs_interpretation_layer_present('5fe9b716-cefb-4346-98fc-8321820c4399').
narrative_ontology:cs_reading_relation('5fe9b716-cefb-4346-98fc-8321820c4399', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fe9b716-cefb-4346-98fc-8321820c4399', catastrophe_memory_survival__hybrid_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('5fe9b716-cefb-4346-98fc-8321820c4399', foundational, ritual_performance_is_knowledge_carriage).
narrative_ontology:cs_axiom_status(ritual_performance_is_knowledge_carriage, holdable).
narrative_ontology:cs_axiom_grounding('5fe9b716-cefb-4346-98fc-8321820c4399', ritual_performance_is_knowledge_carriage, empirically_contingent).
narrative_ontology:cs_axiom('5fe9b716-cefb-4346-98fc-8321820c4399', secondary, content_recovery_outweighs_form_preservation).
narrative_ontology:cs_axiom_status(content_recovery_outweighs_form_preservation, holdable).
narrative_ontology:cs_axiom_grounding('5fe9b716-cefb-4346-98fc-8321820c4399', content_recovery_outweighs_form_preservation, instrumental).
narrative_ontology:cs_reference_frame('5fe9b716-cefb-4346-98fc-8321820c4399', ritual_as_encoded_competence_system).
narrative_ontology:cs_drift_state('5fe9b716-cefb-4346-98fc-8321820c4399', contemporary_content_decay_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fe9b716-cefb-4346-98fc-8321820c4399', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__competence_transmission_reading, clerical_transmission_hierarchy).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, content_atrophied_practicing_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__competence_transmission_reading, cost_burdened_poor_households).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, ritual_as_memory_technology).
narrative_ontology:constraint_vindicates(catastrophe_memory_survival__competence_transmission_reading, embodied_knowledge_durability_under_infrastructure_collapse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Carried the ritual package through displacement and receives its encoded content as working procedure in unfamiliar environments: planting and harvest timing translated to new climate zones, rationing and store-management rules, kin-obligation networks that pool risk and mobilize mutual aid after shocks. They adopt selectively and can drop practices that stop paying; the price of exit is frayed social ties, not ruin.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, diaspora_communities, beneficiary,
    moderate, generational, mobile, global).

% Perform the full calendar of observances in liturgical registers they no longer understand. The encoded agronomic timing and resource rules have drifted out of intelligibility, so they supply rehearsal labor and material cost while receiving little decodable competence back. Leaving the practice would cost membership, marriageability, and burial rights; the community and the practice are the same thing from where they stand.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, content_atrophied_practicing_communities, payer,
    moderate, generational, identity_locked, regional).

% Obligated to fund feasts, offerings, and life-cycle rites at customary scale regardless of harvest quality. Ceremonial debt is a recognized channel into poverty in their communities. Skipping a rite marks the household and invites sanction; scaling down invites unfavorable comparison with better-off neighbors, so the cost falls hardest on those least able to carry it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, cost_burdened_poor_households, payer,
    powerless, biographical, constrained, local).

% Controls decoding, ordains who may transmit, sets the performance calendar, and receives offerings, stipends, and the authority that comes from administering the system. Its members' careers and self-concept are constituted by the transmission office; opening the archive to lay decoding would dissolve the gatekeeping that defines the role, so full transparency is affordable technically and ruinous professionally.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, clerical_transmission_hierarchy, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__competence_transmission_reading, clerical_transmission_hierarchy, beneficiary).

% Left the practice or were born outside it and now face the same displacement and supply-disruption risks as everyone else, with no access to the encoded protocols, which remain reachable only through full adherence. They would argue for publishing the practical content openly; no deliberative body of the tradition seats them.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, secularized_descendants, excluded,
    moderate, biographical, mobile, global).

% Document correlations between ritual calendars and ecological cycles, test oral-tradition hazard warnings against sedimentary and seismic records, and publish on which rites still carry decodable content. They hold no stake in performance; their findings feed reform movements pressing for vernacularization and traditionalist defenses of the intact tradition alike.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__competence_transmission_reading, disaster_ethnographers, observer,
    organized, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_survival__competence_transmission_reading, clerical_transmission_hierarchy).
narrative_ontology:fixing_cost_class(catastrophe_memory_survival__competence_transmission_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the intergenerational transmission problem for rare-catastrophe knowledge: knowledge relevant to events too infrequent for any living generation to have learned by direct experience must be stored in a durable, periodically rehearsed, socially enforced container. The ritual calendar schedules mandatory refresh cycles; performance obligations guarantee re-enactment even in generations that see no catastrophe; the liturgical register binds the content to a carrier resistant to casual alteration.
% TRANSFER_FUNCTION: Moves rehearsal labor, time, and material resources (offerings, feasts, ceremonial goods) from practicing communities and households to the ritual system that sustains the specialist lineages administering it, and moves encoded practical knowledge (timing, rationing, kin-obligation, adaptation protocols) across generations and geographies, with the diaspora wing receiving a large share of decodable adaptive capacity relative to what it pays.
% ABSENT_VOICES: Secularized descendants and uninitiated neighbors are the structural absentees: they bear the same catastrophe exposure but cannot reach the gated protocols without full adherence, and no deliberative body of the tradition seats them. Within practicing communities, the question of what a rite means is routinely deflected as deficient faith rather than received as evidence of transmission failure, so the people best positioned to report content decay are disciplined out of the conversation.
% DISAPPEARANCE_RATIONALE: Overnight disappearance would strand rare-catastrophe knowledge with no rehearsal vehicle: diaspora communities would lose the portable protocol packages that currently give them planting, rationing, and mutual-aid procedures in unfamiliar environments, and origin communities would need to rebuild explicit documentation and drill systems from scratch, a generation-scale project. Performance obligations, ceremonial economies, and the hierarchy's office would all unwind together.
% FOUNDING_PROBLEM: Catastrophes recur on timescales longer than any individual's memory: floods, droughts, famines, and displacements arrive after the last witnesses are dead. Communities that survived needed descendants to act on knowledge nobody alive had personally acquired, when to move, what to store, whom to obligate, and ritual was assembled as the durable, rehearsed, socially enforced container for that knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: disaster ethnography and geology attest that practical content is really encoded, in oral-tradition hazard warnings whose content matches sedimentary and seismic records and in ritual calendars whose timing tracks agronomic and hydrological windows; emergency-management researchers studying indigenous early-warning systems attest the transmission function independent of any clerical interest. What no outside source attests is that contemporary performance still delivers the content at scale; that remainder is exactly what the surviving_content_fraction omega leaves open.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__competence_transmission_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__competence_transmission_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__competence_transmission_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_survival__competence_transmission_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__competence_transmission_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_survival__competence_transmission_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_survival__competence_transmission_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) because the arrangement runs a real transmission service whose costs and benefits are unevenly distributed rather than a pure levy: diaspora seats sit far down the beneficiary side while content-decayed performer seats sit near the full-target end, and the story-level epsilon blends them. Suppression (0.58) is social rather than statist: performance obligations are held by sanction, marriageability, and burial rights, and the practical content is reachable only through full adherence, so exit is priced in identity rather than force. Theater_ratio (0.32) is kept below coin-flip deliberately: on this reading rehearsal is itself the functional mechanism, since periodic refresh is what keeps rare-event knowledge retrievable, so performance is not inherently theater; the measured theater concentrates in form-without-content pockets and rises with liturgical unintelligibility across the interval. Accessibility_collapse (0.42) is moderate: writing, schooling, and emergency drills are genuine alternatives, but embodied transmission retains advantages under infrastructure collapse that documents lack, so alternatives narrow rather than vanish once the mechanism is understood. Resistance (0.45) reflects secularization, selective adoption, and reform movements pressing for vernacularization and open publication. boltzmann.coordination_type is declared information_standard: on this reading the ritual is an encoding-and-refresh protocol for survival-relevant information, and the type's low inherent-cost floor is left untouched so that any extraction above genuine carriage cost surfaces for review rather than being excused as coordination overhead. The measurement series run on one shared seven-point grid (t=0 to 60) with all three tracked metrics authored at every point; the trajectories tell one story: content decay drives extractiveness and theater upward through t=50, enforcement hardens against secularization to 0.61, then vernacularizing reforms and published catechisms relax suppression and claw back a little content by t=60.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the clerical seat the arrangement is stewardship: it built and maintains the only durable carrier of knowledge its members regard as indispensable, and its identity is fused with the transmission office itself (institutional identity fusion, identity_locked exit), so it experiences restriction as preservation. From the content-atrophied performer seat the same calendar operates as uncompensated extraction: full costs, no decodable return, and exit priced in membership, marriage, and burial, a relational identity lock. The diaspora seat computes subsidy-side: it receives protocols it did not pay origin-side costs to produce. The engine derives these divergences from role, power, and exit declarations; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations drive the derivation. diaspora_communities (beneficiary, mobile exit) derive near the beneficiary end: the constraint subsidizes them with adaptive capacity and they can drop practices that stop paying. content_atrophied_practicing_communities (victim, identity_locked) derive near the full-target end: identity lock amplifies effective extraction because exit is structurally unavailable. cost_burdened_poor_households (victim, constrained) sit high-target with less amplification than the identity-locked seat. clerical_transmission_hierarchy (agenda_setter with a secondary beneficiary position, identity_locked) derives low-d but with capture flavor: it administers the system and simultaneously collects offerings, stipends, and authority rents, which is why gain_flow names this seat. Suppression is authored as a raw structural property and is not scaled by anything; extractiveness is what the engine scales by directionality and scope, and the diaspora wing's global reach widens verification distance, modestly amplifying effective extraction on the payer side.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, making descendants act on catastrophe knowledge no living member has personally acquired, is still live and arguably more so in a climate-displacement era, so this is not a classic mandatrophy case at story level: founding_problem_status=live paired with disappearance_verdict=world_rearranges raises no capture flag. The decay is sub-population: measurements show theater climbing toward 0.33 in form-without-content pockets, which is where piton-like maintenance lives. The classification discipline matters in both directions here. Calling the whole arrangement a snare would erase the genuine transmission subsidy the diaspora seat demonstrably receives; calling it a rope would erase the identity-locked performers who pay full price for an empty envelope. Tangled_rope is the honest structural claim: one mechanism, coordinated beneficiaries, identifiable payers, and active enforcement holding the form in place after the content has partially gone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the catastrophe_memory_survival kernel: what structural differences would the sibling readings produce if adopted instead?',
    'Classify the sibling reading-stories and compare victim sets, epsilon, and seat divergence across the kernel family; the hybrid reading''s dual-register decomposition predicts intermediate extraction concentrated where one register dominates.',
    'Under the symbol reading the victim set shifts to communities whose practice-continuity breaks regardless of decodable content; under the hybrid reading epsilon splits between registers and the competence-only victim set shrinks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-level framing ambiguity: which register of ritual carries the survival value.').

omega_variable(
    surviving_content_fraction,
    'What fraction of performed ritual content still carries decodable practical survival knowledge (agronomic timing, rationing rules, hazard protocols)?',
    'Corpus analysis mapping rite elements to ecological and subsistence referents, cross-checked against disaster ethnography and historical records of ritual-calendar accuracy.',
    'If the surviving fraction is small, performance costs in origin communities approach pure theater and the constraint drifts piton-ward; if large, the coordination function dominates and extraction is mostly gatekeeping overhead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surviving_content_fraction, empirical, 'How much decodable practical content actually survives in contemporary performance.').

omega_variable(
    embodied_substitutability,
    'Can explicit documentation and drills substitute for embodied ritual rehearsal, or does ritual carry tacit, habitual knowledge that documents cannot?',
    'Compare disaster-response outcomes between communities that retained ritual transmission and communities that replaced it with written protocols and school drills.',
    'If substitutable, accessibility_collapse is overstated and the suppression backing the gate loses justification; if not, part of the measured extraction is the irreducible price of durable transmission under infrastructure collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_substitutability, empirical, 'Whether explicit documentation can replace embodied rehearsal as a transmission vehicle.').

omega_variable(
    gatekeeping_function_ambiguity,
    'Is restricted decoding a preservation mechanism (context-bound knowledge degrades when extracted from practice) or rent-protecting gatekeeping by the transmission hierarchy?',
    'Track outcomes where traditions published decoded handbooks openly: did practical competence spread without degradation, or did misuse and decontextualization follow?',
    'If preservation, the hierarchy''s restriction is coordination cost and its beneficiary position is partly justified; if gatekeeping, the hierarchy''s extraction component rises and the constraint slides snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_function_ambiguity, empirical, 'Whether the hierarchy''s control of decoding protects the knowledge or the hierarchy.').

omega_variable(
    diaspora_retention_driver,
    'Do diaspora communities retain ritual for the encoded adaptive protocols or for identity anchoring under displacement?',
    'Adoption-pattern studies separating practice elements with decodable survival content from purely symbolic elements, comparing retention rates across content-rich and content-poor rites.',
    'If retention tracks content, the beneficiary declaration stands; if it tracks identity alone, the diaspora seat migrates toward the symbol reading''s structure and this reading''s beneficiary set thins.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_retention_driver, empirical, 'What actually drives diaspora practice retention: content or identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__competence_transmission_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 10, 0.21).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 50, 0.33).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__competence_transmission_reading, theater_ratio, 60, 0.32).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 50, 0.53).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__competence_transmission_reading, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 50, 0.61).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__competence_transmission_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__competence_transmission_reading, information_standard).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__competence_transmission_reading, catastrophe_memory_survival__hybrid_encoding_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'ritual preserves communities through catastrophe' decomposes, per the epsilon-invariance principle, into three structurally distinct claims with different epsilon, victim sets, and failure modes: this competence reading (practical-content carriage; victims are form-without-content performers), symbol_survival_reading (identity and boundary continuity; victims are communities whose practice-chain breaks), and hybrid_encoding_reading (dual-register operation; extraction splits by register). The symbol reading is the older, more established framing in religious studies and functions upstream; the competence reading draws on disaster ethnography and exerts structural pressure on the hybrid reading's practical register. Each file links the other two via affects_constraints; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
