% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__strategic_deployment, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: press_reformation_causation__strategic_deployment
 *   human_readable: Strategic Deployment of the Press in the Reformation (Agency-Upstream Reading)
 *   domain: history of technology / religious history / media studies
 *
 * SUMMARY:
 *   This constraint story instantiates the strategic_deployment reading of
 *   the press_reformation_causation kernel: the claim that Protestant
 *   reformers and commercial printers actively exploited the printing press
 *   as a neutral tool, deliberately deploying it to extract profit and
 *   religious-political authority. The press itself functions as a
 *   coordination mechanism (rope-like information dissemination), while the
 *   reformer-printer alliance introduces asymmetric extraction, producing a
 *   tangled_rope structure. The story is authored as one clean, Îµ-invariant
 *   constraint; sibling readings (technological_determinism, mutual_shaping)
 *   are referenced only in the cs_structure and network blocks per Rule 1.
 *
 * KEY AGENTS:
 *   - reformist_printers: Agenda-setter/beneficiary (organized/constrained) â physically produces and distributes texts, captures printing profits
 *   - protestant_reformers: Agenda-setter/beneficiary (powerful/identity_locked) â directs content and strategy, captures authority and allegiance
 *   - roman_catholic_hierarchy: Primary target (institutional/identity_locked) â loses interpretive monopoly and tithe allegiance
 *   - urban_scribal_guilds: Secondary target (moderate/trapped) â lose livelihood to mechanical reproduction
 *   - territorial_catholic_rulers: Secondary target (powerful/constrained) â lose confessional-political control
 *   - vernacular_readership: Coordinated beneficiary (powerless/constrained) â gains access but is shaped by the partisan output filter
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, 0.52).
domain_priors:suppression_score(press_reformation_causation__strategic_deployment, 0.58).
domain_priors:theater_ratio(press_reformation_causation__strategic_deployment, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, extractiveness, 0.52).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__strategic_deployment, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causation__strategic_deployment, "Strategic Deployment of the Press in the Reformation (Agency-Upstream Reading)").
narrative_ontology:topic_domain(press_reformation_causation__strategic_deployment, "history of technology / religious history / media studies").

domain_priors:requires_active_enforcement(press_reformation_causation__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__strategic_deployment, 'a232988d-8ce9-4d5d-a06d-8a4cafd0d923').
narrative_ontology:cs_kernel_codification('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', distributed).
narrative_ontology:cs_authority_grounding('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', expertise).
narrative_ontology:cs_interpretation_layer_present('a232988d-8ce9-4d5d-a06d-8a4cafd0d923').
narrative_ontology:cs_reading_relation('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', press_reformation_causation__mutual_shaping, coexists_with).
narrative_ontology:cs_axiom('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', foundational, technology_neutral_awaiting_use).
narrative_ontology:cs_axiom_status(technology_neutral_awaiting_use, holdable).
narrative_ontology:cs_axiom_grounding('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', technology_neutral_awaiting_use, empirically_contingent).
narrative_ontology:cs_axiom('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', foundational, agency_precedes_structure).
narrative_ontology:cs_axiom_status(agency_precedes_structure, holdable).
narrative_ontology:cs_axiom_grounding('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', agency_precedes_structure, empirically_contingent).
narrative_ontology:cs_reference_frame('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', agent_centered_historiography).
narrative_ontology:cs_drift_state('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', contemporary_media_studies_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a232988d-8ce9-4d5d-a06d-8a4cafd0d923', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__strategic_deployment, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, reformist_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__strategic_deployment, vernacular_readership).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, roman_catholic_hierarchy).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, urban_scribal_guilds).
narrative_ontology:constraint_victim(press_reformation_causation__strategic_deployment, territorial_catholic_rulers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and operate print shops, select which tracts and Bibles to produce in what impressions, and actively manage trans-local distribution networks across German-speaking territories and beyond. They profit from each sale and often enjoy political protection from reform-friendly city councils. Their capital is sunk in type and presses, and guild membership restricts easy occupational exit.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, reformist_printers, agenda_setter,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__strategic_deployment, reformist_printers, beneficiary).

% Theologians and preachers who strategize the content, timing, and target audiences of printed polemic. They provide manuscripts, secure financing through patrons, and direct printers toward specific doctrinal and political goals. Their public authority grows with each successful imprint. Renouncing the cause would mean abandoning their theological identity, patronage networks, and the movement they helped build.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, protestant_reformers, agenda_setter,
    powerful, generational, identity_locked, continental).

% Bishops, curial officials, and religious orders whose centuries-old monopoly on interpreting sacred doctrine and controlling sacred texts is eroded by unauthorized vernacular printing. They bear the cost of lost tithe allegiance, heresy proliferation, and the mounting administrative and financial burden of counter-reformation censorship, indexing, and theological response. Their institutional identity is locked to defending the traditional media regime.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, roman_catholic_hierarchy, payer,
    institutional, civilizational, identity_locked, continental).

% Copyists, illuminators, and stationers whose livelihood depended on manuscript production for ecclesiastical, university, and noble clients. As printed pamphlets and Bibles flooded urban markets at lower unit cost, demand for their specialized craft collapsed. Guild restrictions on occupational mobility and the absence of alternative training left many trapped in a declining trade.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, urban_scribal_guilds, payer,
    moderate, biographical, trapped, regional).

% Merchants, artisans, and literate laypeople who gained access to comparatively cheap vernacular Bibles, pamphlets, and woodcut propaganda. They are coordinated into a trans-local religious public through shared printed texts. While they benefit from information access, their choice of material is constrained by what the reformer-printer network chooses to produce and what local magistrates permit.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, vernacular_readership, beneficiary,
    powerless, biographical, constrained, continental).

% Secular rulers in Catholic territories who face destabilization of the confessional unity that underpinned their legitimacy, legal systems, and social control. They pay the cost of suppressing print-fueled unrest, maintaining censorship apparatus, and lost tax revenue from Church lands and benefices shifting to Protestant princes. Their political options are constrained by imperial law, dynastic alliances, and the threat of princely defection.
narrative_ontology:constraint_stakeholder(press_reformation_causation__strategic_deployment, territorial_catholic_rulers, payer,
    powerful, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a reproducible, trans-local infrastructure for disseminating vernacular religious texts and political propaganda, replacing manuscript bottlenecks with rapid, standardized multiplication of messages across the Holy Roman Empire and beyond.
% TRANSFER_FUNCTION: Moves printing profit from readers and patrons to commercial printers; moves religious authority and political allegiance from the Catholic hierarchy and territorial rulers to Protestant reformers and their allied magistrates.
% ABSENT_VOICES: Illiterate rural peasants, women excluded from Latin and public print culture, Anabaptist radicals whose own press campaigns were crushed or co-opted by mainstream reformers, and Catholic lay brotherhoods whose devotional reading practices were overwritten by the polemical canon.
% DISAPPEARANCE_RATIONALE: If the reformer-printer strategic alliance and its legitimizing 'neutral tool' framing disappeared, the flood of vernacular Bibles and pamphlets would slow to a trickle; manuscript culture would partially recover in the short term, Catholic information control would regain ground in contested territories, and the extracted profit and authority would revert to traditional scribal and ecclesiastical seats.
% FOUNDING_PROBLEM: Pre-Reformation Europe lacked any rapid, affordable, and wide-reaching medium for vernacular theological dissent or cross-border religious mobilization; manuscript transmission was too slow, too expensive, and too institutionally controlled to challenge the Catholic hierarchy's interpretive monopoly.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians of the book trade and independent city-council archives attest the scarcity and cost of manuscripts. Catholic controversialists and later media theorists from outside the reformer-printer beneficiary set attest that the strategic deployment arrangement outlived the scarcity problem and became a self-sustaining extraction and identity-formation machine well before mid-century.
narrative_ontology:disappearance_verdict(press_reformation_causation__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__strategic_deployment, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__strategic_deployment, 0.52, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causation__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causation__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the press genuinely solves a coordination problem (mass vernacular communication) even as reformers and printers capture disproportionate profit and authority. Suppression is moderate-high (0.58): the arrangement actively suppresses Catholic counter-printing in Protestant territories and drives scribal alternatives to commercial collapse through price competition, though it does not fully eliminate them. Theater ratio starts low (0.20) and rises to 0.35 as the 'neutral tool' framing becomes increasingly performative while deployment grows more partisan. Resistance is substantial (0.65) because the Catholic hierarchy and imperial authorities mounted active censorship, indexing, and theological counter-campaigns. The temporal series share a single grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda_setter seats (printers, reformers) experience the constraint as a coordination device they built and operate; the victim seats (Catholic hierarchy, scribes, Catholic rulers) experience it as an extractive attack on their authority and livelihood; the readership experiences it as empowerment with hidden filtering. The engine computes this divergence from structural data rather than authored perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist_printers and protestant_reformers are declared beneficiaries and agenda_setters with constrained or identity_locked exit, yielding low directionality (they are subsidized by the constraint). Vernacular_readership is also a beneficiary, though powerless and constrained, yielding low-to-moderate directionality. Roman_catholic_hierarchy, urban_scribal_guilds, and territorial_catholic_rulers are declared victims: the hierarchy is institutional and identity_locked, guilds are moderate and trapped, rulers are powerful but constrained â all yielding high directionality. The engine will compute high effective extraction for the victim seats and low or negative extraction for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â scarcity of vernacular theological media â was solved by the press within the first decade. Yet the arrangement persisted and intensified because the reformer-printer network had become identity-locked and commercially dependent on the extraction stream. The R5 genealogy (founding_problem_status: dead) prevents mislabeling the mature constraint as a still-needed scaffold. Without the R5 interview, the constraint might be mistaken for a rope or scaffold; with it, the persistence of extraction after the founding problem's death flags the tangled_rope dynamic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    press_neutrality_ambiguity,
    'Does the printing press function as a genuinely neutral tool awaiting purposeful use, or do its technical affordances (standardization, volume, fixity) structurally favor certain genres, arguments, and political outcomes regardless of agent intent?',
    'Comparative bibliometric and discourse analysis of print output across Catholic, Protestant, and secular contexts in the first half of the sixteenth century to test whether the same technology produced systematically different results under divergent agent purposes.',
    'If the press is materially non-neutral, the strategic_deployment reading overstates agency and the constraint edges toward a more deterministic or tangled classification; if neutral, the rope coordination function is vindicated and extraction is more clearly attributable to agent strategy alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(press_neutrality_ambiguity, conceptual, 'Whether the press is materially neutral or structurally biased.').

omega_variable(
    extraction_vs_coordination_proportion,
    'What proportion of printer profit and reformer authority in the early Reformation derived from genuine coordination cost (replacing manuscript inefficiency) versus asymmetric extraction (censorship-evasion premium, partisan monopoly, captive readership)?',
    'Economic analysis of print shop margins in reformer-protected versus open-market cities, combined with network analysis of distribution monopolies.',
    'A high extraction proportion confirms tangled_rope and raises effective extraction for victim seats; a low proportion would push classification toward rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_vs_coordination_proportion, empirical, 'Balance of coordination benefit to extractive rent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression measured in this constraint primarily structural (external censorship, guild barriers, legal threats) or internalized (scribal copyists believing their craft was naturally obsolete, Catholic authorities accepting the inevitability of print)?',
    'Analysis of post-exit trajectories: if suppression of scribal alternatives persists after legal barriers fall, reclassify as partially internalized; if suppression vanishes with legal change, it was structural.',
    'If internalized, effective suppression exceeds the structural measure; the victim seats carry the constraint even after external enforcement weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__strategic_deployment, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_strat_tr_t0, press_reformation_causation__strategic_deployment, theater_ratio, 0, 0.2).
narrative_ontology:measurement(press_ref_strat_tr_t10, press_reformation_causation__strategic_deployment, theater_ratio, 10, 0.24).
narrative_ontology:measurement(press_ref_strat_tr_t20, press_reformation_causation__strategic_deployment, theater_ratio, 20, 0.28).
narrative_ontology:measurement(press_ref_strat_tr_t30, press_reformation_causation__strategic_deployment, theater_ratio, 30, 0.3).
narrative_ontology:measurement(press_ref_strat_tr_t40, press_reformation_causation__strategic_deployment, theater_ratio, 40, 0.32).
narrative_ontology:measurement(press_ref_strat_tr_t50, press_reformation_causation__strategic_deployment, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(press_ref_strat_be_t0, press_reformation_causation__strategic_deployment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(press_ref_strat_be_t10, press_reformation_causation__strategic_deployment, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(press_ref_strat_be_t20, press_reformation_causation__strategic_deployment, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(press_ref_strat_be_t30, press_reformation_causation__strategic_deployment, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(press_ref_strat_be_t40, press_reformation_causation__strategic_deployment, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(press_ref_strat_be_t50, press_reformation_causation__strategic_deployment, base_extractiveness, 50, 0.55).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__strategic_deployment, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__strategic_deployment, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The natural-language label 'the press caused the Reformation' conflates three structurally distinct claims: technological determinism (press as independent cause), strategic deployment (agency exploiting neutral tool â this story), and mutual shaping (co-evolution of technology and practice). Each reading has a distinct epsilon, beneficiary structure, and classification. This story instantiates the strategic_deployment reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
