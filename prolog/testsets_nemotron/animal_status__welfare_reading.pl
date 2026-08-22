% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Welfare Regulation of Animal Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   The welfare reading of animal status holds that animals are sentient
 *   beings with interests that constrain but do not prohibit human use. This
 *   reading grounds the modern animal welfare regulatory regime: anti-cruelty
 *   statutes, husbandry standards, laboratory animal care requirements, and
 *   the 3Rs framework (Replacement, Reduction, Refinement). The constraint
 *   coordinates a baseline of humane treatment while preserving the
 *   institutional legitimacy of animal agriculture, biomedical research, and
 *   other instrumental uses. However, the regulatory architecture contains
 *   extensive exemption structures — agricultural exceptions for 'customary
 *   farming practices,' research exceptions for 'scientific necessity,' and
 *   enforcement gaps that concentrate protection on companion animals while
 *   farmed and laboratory animals receive minimal substantive protection. The
 *   constraint extracts via these exemptions: the welfare framework
 *   legitimates the continued expansion of industrial animal use by providing
 *   a moral and legal cover that the system is 'humane,' while the exemptions
 *   ensure the economic logic of intensive use remains undisturbed.
 *   Extraction has risen from ~0.12 (early anti-cruelty laws targeting
 *   sadistic acts) to ~0.45 (comprehensive regulatory regime with
 *   industrial-scale exemptions), and theater has risen from ~0.08 to ~0.38
 *   as welfare performance (inspections, protocols, certifications)
 *   increasingly substitutes for substantive protection.
 *
 * KEY AGENTS:
 *   - animal_agriculture_industry: Primary beneficiary (institutional/arbitrage) — gains legitimacy and regulatory capture from welfare framework while exemptions preserve extraction
 *   - biomedical_research_establishment: Primary beneficiary (institutional/arbitrage) — gains social license and funding stability from 3Rs framework while 'scientific necessity' exemptions preserve experimental freedom
 *   - regulatory_agencies: Agenda setter (institutional/arbitrage) — administers welfare standards, captures enforcement discretion, rotates with industry
 *   - farmed_animals: Primary victim (powerless/trapped) — bears 99%+ of animal use by biomass; welfare protections minimal, exemptions near-total
 *   - laboratory_animals: Primary victim (powerless/trapped) — bears concentrated harm in research; 3Rs framework aspirational, 'necessity' exemption broad
 *   - animal_advocacy_organizations: Payer/beneficiary dual (organized/constrained) — extracts concessions through litigation and campaigns; co-opted into welfare framework
 *   - consumers: Beneficiary/payer dual (organized/constrained) — gains 'humane' labeling assurance; pays price premiums that flow to industry not animals
 *   - philosophical_observers: Observer (analytical/analytical) — evaluates structural coherence of welfare premise against abolitionist and property alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.52).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Welfare Regulation of Animal Use").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, 'fd89259c-2d95-43db-aff0-b44459588fac').
narrative_ontology:cs_kernel_codification('fd89259c-2d95-43db-aff0-b44459588fac', distributed).
narrative_ontology:cs_authority_grounding('fd89259c-2d95-43db-aff0-b44459588fac', distributed).
narrative_ontology:cs_reading_relation('fd89259c-2d95-43db-aff0-b44459588fac', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fd89259c-2d95-43db-aff0-b44459588fac', animal_status__property_reading, influences).
narrative_ontology:cs_axiom('fd89259c-2d95-43db-aff0-b44459588fac', foundational, sentience_grounds_interests_that_constrain_use).
narrative_ontology:cs_axiom_status(sentience_grounds_interests_that_constrain_use, holdable).
narrative_ontology:cs_axiom_grounding('fd89259c-2d95-43db-aff0-b44459588fac', sentience_grounds_interests_that_constrain_use, deontological).
narrative_ontology:cs_axiom('fd89259c-2d95-43db-aff0-b44459588fac', foundational, instrumental_use_permissible_with_welfare_provisions).
narrative_ontology:cs_axiom_status(instrumental_use_permissible_with_welfare_provisions, holdable).
narrative_ontology:cs_axiom_grounding('fd89259c-2d95-43db-aff0-b44459588fac', instrumental_use_permissible_with_welfare_provisions, conventional).
narrative_ontology:cs_reference_frame('fd89259c-2d95-43db-aff0-b44459588fac', anti_cruelty_legitimacy_framework).
narrative_ontology:cs_drift_state('fd89259c-2d95-43db-aff0-b44459588fac', industrial_animal_use_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fd89259c-2d95-43db-aff0-b44459588fac', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_research_establishment).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, regulatory_agencies).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, laboratory_animals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_advocacy_organizations).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, consumers).
narrative_ontology:constraint_victim(animal_status__welfare_reading, animal_advocacy_organizations).
narrative_ontology:constraint_victim(animal_status__welfare_reading, consumers).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, sentience_criterion_for_moral_consideration).
narrative_ontology:constraint_vindicates(animal_status__welfare_reading, proportionality_principle_in_animal_use).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives regulatory legitimacy, social license, and marketing assets ('humane certified') from the welfare framework. Agricultural exemptions for 'customary farming practices' preserve intensive confinement, mutilations without analgesia, and early weaning. Industry dominates standard-setting bodies and rotates personnel with regulatory agencies. Exit is arbitrage-grade: can relocate production, lobby for weaker standards, or capture certification schemes.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Gains public trust, funding stability, and ethical cover from the 3Rs framework and Institutional Animal Care and Use Committees (IACUCs). 'Scientific necessity' exemptions are broadly interpreted; protocol review is internal. Non-animal methods receive marginal funding. Exit is arbitrage-grade: can outsource to jurisdictions with weaker standards, define 'necessity' expansively, and control validation pathways for alternatives.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_research_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Administers welfare standards (USDA APHIS, NIH OLAW, EU DG SANTE), controls enforcement discretion, and staffs from industry. Mandates are dual: promote animal welfare AND promote animal agriculture/research. Revolving-door employment creates structural alignment with regulated industries. Exit is arbitrage-grade: career paths flow between agencies, industry, and consulting.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, regulatory_agencies, agenda_setter,
    institutional, generational, arbitrage, national).

% Bears 99%+ of animal use by biomass (70+ billion land animals/year). Welfare protections minimal: US '28 Hour Law' and Humane Slaughter Act exclude poultry; EU directives permit intensive confinement; 'customary farming practice' exemptions override standards. No legal standing, no political voice, biological confinement. Exit is trapped: bred into the system, no alternative exists within the constraint.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Bears concentrated harm in research (100+ million/year globally). 3Rs framework is aspirational: Replacement receives <1% of research funding; Reduction is offset by increasing study numbers; Refinement applies only when 'scientifically compatible.' IACUC review is internal, approval rates >98%. 'Scientific necessity' exemption is self-certified by researchers. Exit is trapped: bred for purpose, no legal personhood, no release pathway.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, global).

% Extracts incremental concessions (cage-free pledges, funding for alternatives) through litigation, campaigns, and corporate pressure. Simultaneously co-opted: welfare reforms become the ceiling of achievable change; 'pragmatic' organizations gain access and funding while abolitionist demands are marginalized. Exit is constrained: can push for stronger welfare or shift to abolitionist framing, but the welfare framework structures the discourse and funding landscape.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_advocacy_organizations, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, animal_advocacy_organizations, beneficiary).

% Gains psychological assurance from 'humane' labeling and welfare certifications. Pays price premiums for welfare-labeled products that flow to industry margins, not animal welfare. Choice architecture is structured by the welfare framework: 'humane' options exist within the system; opting out requires rejecting animal products entirely. Exit is constrained: plant-based alternatives exist but are disadvantaged by subsidies, labeling rules, and cultural normalization of animal use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, consumers, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_status__welfare_reading, consumers, payer).

% Evaluates the structural coherence of the welfare premise against abolitionist and property alternatives. Analyzes whether the sentience/interests criterion logically entails the victim-set boundary the welfare reading draws, or whether the distinction between 'gratuitous' and 'instrumental' harm collapses under the premise's own logic. Neither collects nor pays; provides the analytical seat for classification.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, philosophical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a socially accepted baseline of humane treatment for animals, resolving the coordination problem of public opposition to cruelty while preserving the institutional legitimacy of animal agriculture and biomedical research. Solves: 'How can society use animals at scale without appearing barbaric?'
% TRANSFER_FUNCTION: Moves legitimacy, social license, and regulatory stability from the public (concerned about cruelty) to animal-use industries (agriculture, research). Moves welfare costs (compliance, certification, marginal housing improvements) from industries to consumers via price premiums. Moves harm (confinement, suffering, death) from industries to animals via exemption structures that preserve intensive use.
% ABSENT_VOICES: Animals themselves are structurally excluded — they cannot testify, litigate, or vote. Future generations of animals (who will be born into the system) are excluded. Abolitionist advocates are excluded from regulatory standard-setting bodies. Non-animal method developers are excluded from validation pathways controlled by animal-use establishments. These voices would challenge the victim-set boundary and the exemption architecture.
% DISAPPEARANCE_RATIONALE: If the welfare constraint vanished overnight, animal agriculture and research would lose their primary legitimacy mechanism. Public opposition would surge without the 'humane' cover. Industries would face direct pressure to abolish or radically transform. Legal property status of animals would be exposed without the welfare mediation. The world would rearrange toward either abolitionist pressure or property-reading regression — the welfare framework is the structural dam holding both at bay.
% FOUNDING_PROBLEM: Early 19th century: preventing sadistic cruelty to animals (bear-baiting, horse-beating, gratuitous violence) which was seen as degrading to human character and public morals. The founding problem was cruelty as a vice, not animal use as such.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (Ritvo, Thomas, Francione) attests that the founding problem — sadistic cruelty as a public vice — was substantially addressed by early anti-cruelty laws (Martin's Act 1822, subsequent statutes). The modern welfare regime addresses a different problem: managing opposition to industrial-scale instrumental use. The animal-use industries (beneficiaries) claim the problem is still live (ongoing cruelty); abolitionist and independent scholars (outside beneficiaries) attest the problem has shifted. No corroboration from outside the beneficiary set supports 'live' status.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects the scale of animal use legitimated by the welfare framework relative to the marginal welfare gains delivered. The constraint coordinates genuine harm reduction (anti-cruelty baseline, some husbandry improvements) but extracts massively through exemptions that preserve the economic core of industrial use. Suppression (0.52) operates through legal categorization (animals as property), regulatory capture (industry-dominated standard-setting), and epistemic closure (welfare science framed as 'sufficient' without addressing use itself). Theater (0.38) is rising: inspection regimes, certification programs, and transparency initiatives create appearance of oversight while exemption structures remain intact. Accessibility collapse (0.42) is moderate: alternatives exist (plant-based systems, non-animal methods) but are structurally disadvantaged by the welfare framework's legitimation of the status quo. Resistance (0.31) is growing but fragmented: advocacy campaigns, litigation, and market shifts meet institutional inertia and regulatory capture.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (regulatory agencies), the constraint appears as genuine coordination: it solves the problem of public demand for humane treatment while preserving food security and medical progress. From the victim seats (farmed animals, laboratory animals), the same structure operates as extraction with coordination theater: their interests are acknowledged in principle but overridden by exemption structures in practice. From the beneficiary seats (industry, research), the constraint is a favorable settlement: it absorbs advocacy pressure, provides marketing assets ('humane'), and stabilizes the regulatory environment. The engine computes these divergent per-seat classifications from the structural data — the claimed_type (tangled_rope) represents the generating model's structural assessment, not a reconciliation of perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (industry, research, agencies) collect rents: legitimacy, regulatory capture, social license, revolving-door careers. Their directionality d is low (beneficiary end). Victims (farmed animals, laboratory animals) bear concentrated harm with near-zero exit: biological confinement, legal property status, no political voice. Their d is near 1.0 (full target). Dual-positioned agents (advocacy orgs, consumers) sit near symmetric (d ~0.5): they gain some coordination benefit (reduced gratuitous cruelty) but pay costs (advocacy resources, price premiums) and are constrained by the framework's legitimation function. Observers sit at analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing sadistic cruelty to animals — was substantially addressed by early anti-cruelty laws (1822-1900). The modern welfare regulatory regime (post-1966) addresses a different problem: managing public opposition to industrial animal use while preserving that use. The mandate has atrophied from 'prevent cruelty' to 'legitimate industrial use through welfare performance.' This is not a pure snare (coordination function is real but narrow) nor a pure rope (extraction is structural and large). Tangled Rope captures the hybrid: genuine coordination of baseline humane treatment AND asymmetric extraction via exemption structures that preserve the economic logic of intensive use. The classification prevents mislabeling the coordination as pure extraction (which would miss the real harm reduction) or the extraction as pure coordination (which would miss the industrial-scale victimization).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint one reading of the contested animal_status kernel, and does the welfare reading instantiate a distinct constraint from the abolitionist and property readings?',
    'Structural analysis: the three readings produce different victim sets, different extractiveness profiles, and different exemption structures — each reading instantiates its own ε-invariant constraint per DP-001.',
    'If readings are structurally distinct constraints, each must be authored as a separate story with its own ε, beneficiaries, victims, and classification. Conflating them produces ε-invariance violation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Whether the welfare reading is a structurally distinct constraint from sibling readings of the animal_status kernel.').

omega_variable(
    welfare_exemption_extraction_boundary,
    'Where does the welfare constraint''s coordination function end and its extraction function begin — specifically, are the exemption structures (agricultural exceptions, research exceptions, customary practice carve-outs) necessary for the coordination of humane treatment, or are they extraction mechanisms that capture the regulatory apparatus?',
    'Comparative regulatory analysis: jurisdictions with narrower exemptions vs. broader exemptions — if animal welfare outcomes do not degrade when exemptions are narrowed, the exemptions are extractive rather than coordinative.',
    'If exemptions are extractive, the constraint''s effective extraction is higher than its welfare provisions alone suggest, and the Tangled Rope classification is reinforced. If exemptions are coordinative, the constraint approaches a purer Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_exemption_extraction_boundary, empirical, 'Whether exemption structures in welfare regulation serve coordination or extraction.').

omega_variable(
    sentience_interests_victim_set_boundary,
    'Does the welfare reading''s victim set properly include animals subjected to gratuitous harm but exclude animals in instrumental use with welfare protections, or does the sentience/interests premise structurally require a broader victim set?',
    'Logical analysis of the welfare premise: if sentience grounds interests that constrain use, does the distinction between ''gratuitous'' and ''instrumental'' harm hold, or does it collapse under the premise''s own logic?',
    'If the premise collapses, the welfare reading either forecloses into abolitionist_reading or reveals itself as a false summit masking property_reading extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_interests_victim_set_boundary, conceptual, 'Whether the welfare reading''s victim set boundary is structurally stable given its own premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1822, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1822, animal_status__welfare_reading, theater_ratio, 1822, 0.08).
narrative_ontology:measurement(anim_tr_t1876, animal_status__welfare_reading, theater_ratio, 1876, 0.12).
narrative_ontology:measurement(anim_tr_t1911, animal_status__welfare_reading, theater_ratio, 1911, 0.15).
narrative_ontology:measurement(anim_tr_t1966, animal_status__welfare_reading, theater_ratio, 1966, 0.22).
narrative_ontology:measurement(anim_tr_t1980, animal_status__welfare_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(anim_tr_t2000, animal_status__welfare_reading, theater_ratio, 2000, 0.33).
narrative_ontology:measurement(anim_tr_t2024, animal_status__welfare_reading, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(anim_be_t1822, animal_status__welfare_reading, base_extractiveness, 1822, 0.12).
narrative_ontology:measurement(anim_be_t1876, animal_status__welfare_reading, base_extractiveness, 1876, 0.18).
narrative_ontology:measurement(anim_be_t1911, animal_status__welfare_reading, base_extractiveness, 1911, 0.22).
narrative_ontology:measurement(anim_be_t1966, animal_status__welfare_reading, base_extractiveness, 1966, 0.28).
narrative_ontology:measurement(anim_be_t1980, animal_status__welfare_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(anim_be_t2000, animal_status__welfare_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(anim_be_t2024, animal_status__welfare_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1822, animal_status__welfare_reading, suppression_requirement, 1822, 0.15).
narrative_ontology:measurement(anim_su_t1876, animal_status__welfare_reading, suppression_requirement, 1876, 0.22).
narrative_ontology:measurement(anim_su_t1911, animal_status__welfare_reading, suppression_requirement, 1911, 0.28).
narrative_ontology:measurement(anim_su_t1966, animal_status__welfare_reading, suppression_requirement, 1966, 0.38).
narrative_ontology:measurement(anim_su_t1980, animal_status__welfare_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(anim_su_t2000, animal_status__welfare_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(anim_su_t2024, animal_status__welfare_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.1).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% The animal_status kernel decomposes into three constraint stories: welfare_reading (this file), abolitionist_reading, and property_reading. The welfare reading cites the sentience criterion (shared with abolitionist) to constrain use, but the property reading's legal categorization (animals as property) provides the structural platform for the exemption architecture that enables extraction. The welfare reading is downstream of the property reading's legal categorization and upstream of the abolitionist reading's challenge to the instrumental-use premise. network.affects_constraints links this reading to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(animal_status__welfare_reading, institutional, 0.15).
constraint_indexing:directionality_override(animal_status__welfare_reading, powerless, 0.95).
constraint_indexing:directionality_override(animal_status__welfare_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
