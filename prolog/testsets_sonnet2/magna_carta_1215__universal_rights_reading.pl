% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__universal_rights_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_1215__universal_rights_reading
 *   human_readable: Magna Carta Clause 39 as Universal Due Process Precedent
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This story generates the universal-rights reading of the Magna Carta
 *   kernel: the claim that Clause 39's 'nullus liber homo' ('no free man')
 *   should be read as proto-language for universal due process protection
 *   against arbitrary state power, applicable to all persons regardless of
 *   the clause's original 1215 scope. This is one of three structurally
 *   distinct readings of the same kernel text. The baronial_privilege_reading
 *   holds that 'free men' meant landowning barons only and the protection set
 *   was limited to the contracting parties — that reading is NOT this
 *   constraint; it is a sibling story. The living_document_reading holds that
 *   original meaning is legitimately superseded by interpretive accumulation
 *   — also not this constraint. This story's ε is authored specifically for
 *   the universal-rights reading's own account of the standing arrangement:
 *   the citation practice by which Clause 39 is invoked as transhistorical
 *   authority for due process claims extending to persons the original
 *   charter did not contemplate.
 *
 * KEY AGENTS:
 *   - modern_constitutional_litigants: beneficiary of the citation chain (moderate/constrained) — invoke ancient pedigree for present claims
 *   - human_rights_advocacy_organizations: agenda_setter/beneficiary (organized/mobile) — actively construct and circulate the universal reading
 *   - common_law_judiciaries: agenda_setter (institutional/arbitrage) — select and apply the reading in precedent
 *   - detained_persons_under_executive_power: payer (powerless/trapped) — bear the gap between rhetorical and enforced protection
 *   - colonized_populations_excluded_from_original_scope: payer (powerless/trapped) — governed under a legal order that invoked Magna Carta's authority while denying them its protections
 *   - historically_marginalized_groups_retrofitted_into_free_men: payer/beneficiary (powerless/constrained) — excluded originally, included rhetorically now
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__universal_rights_reading, 0.42).
domain_priors:suppression_score(magna_carta_1215__universal_rights_reading, 0.38).
domain_priors:theater_ratio(magna_carta_1215__universal_rights_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__universal_rights_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__universal_rights_reading, "Magna Carta Clause 39 as Universal Due Process Precedent").
narrative_ontology:topic_domain(magna_carta_1215__universal_rights_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__universal_rights_reading, 'e4a7738b-77f7-4838-b042-b9d840ef43fb').
narrative_ontology:cs_kernel_codification('e4a7738b-77f7-4838-b042-b9d840ef43fb', fixed_text).
narrative_ontology:cs_authority_grounding('e4a7738b-77f7-4838-b042-b9d840ef43fb', lineage).
narrative_ontology:cs_interpretation_layer_present('e4a7738b-77f7-4838-b042-b9d840ef43fb').
narrative_ontology:cs_reading_relation('e4a7738b-77f7-4838-b042-b9d840ef43fb', magna_carta_1215__baronial_privilege_reading, forecloses).
narrative_ontology:cs_reading_relation('e4a7738b-77f7-4838-b042-b9d840ef43fb', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('e4a7738b-77f7-4838-b042-b9d840ef43fb', foundational, free_men_denotes_all_persons).
narrative_ontology:cs_axiom_status(free_men_denotes_all_persons, holdable).
narrative_ontology:cs_axiom_grounding('e4a7738b-77f7-4838-b042-b9d840ef43fb', free_men_denotes_all_persons, deontological).
narrative_ontology:cs_axiom('e4a7738b-77f7-4838-b042-b9d840ef43fb', foundational, due_process_protection_transcends_original_drafting_scope).
narrative_ontology:cs_axiom_status(due_process_protection_transcends_original_drafting_scope, holdable).
narrative_ontology:cs_axiom_grounding('e4a7738b-77f7-4838-b042-b9d840ef43fb', due_process_protection_transcends_original_drafting_scope, conventional).
narrative_ontology:cs_reference_frame('e4a7738b-77f7-4838-b042-b9d840ef43fb', universal_natural_rights_continuity).
narrative_ontology:cs_drift_state('e4a7738b-77f7-4838-b042-b9d840ef43fb', post_colonial_reckoning_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e4a7738b-77f7-4838-b042-b9d840ef43fb', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__universal_rights_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, modern_constitutional_litigants).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, human_rights_advocacy_organizations).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, common_law_judiciaries).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, detained_persons_under_executive_power).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, colonized_populations_excluded_from_original_scope).
narrative_ontology:constraint_victim(magna_carta_1215__universal_rights_reading, historically_marginalized_groups_retrofitted_into_free_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__universal_rights_reading, historically_marginalized_groups_retrofitted_into_free_men).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke Clause 39's 'law of the land' language, transmitted through Coke, Blackstone, and colonial charters, as authority for due process claims against executive detention or punishment. They benefit from a citation chain that gives their claim the weight of an 800-year pedigree, regardless of whether the original clause contemplated anyone like them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, modern_constitutional_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Actively construct and circulate the universal-rights reading in litigation briefs, textbooks, and international human rights instruments, treating 'free men' as proto-language for 'all persons.' They administer the reading's transmission and have professional and institutional stakes in its continued authority; the reading is a tool they wield, not merely inherit.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, human_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, human_rights_advocacy_organizations, agenda_setter).

% Cite Clause 39 as foundational precedent when adjudicating due process questions, selecting which historical meaning to foreground. They can expand or narrow the universal reading's scope through case law, and their institutional legitimacy is partly built on the claim of unbroken descent from 1215.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, common_law_judiciaries, agenda_setter,
    institutional, civilizational, arbitrage, national).

% Rely on the universal reading to challenge indefinite detention, but the reading's actual protective force depends entirely on whether courts and executives choose to honor it in the moment of crisis — historically the clause has been suspended or ignored precisely when detained persons most needed it (wartime internment, counterterrorism detention), revealing the gap between the rhetorical universality and enforced protection.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, detained_persons_under_executive_power, payer,
    powerless, immediate, trapped, national).

% Were governed by legal systems that invoked Magna Carta's authority to legitimate colonial rule while simultaneously denying colonized subjects the due process protections the universal reading claims Clause 39 guarantees. They bear the cost of a rights narrative that was selectively applied to exclude them even as it was cited as proof of the colonizer's superior legal civilization.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, colonized_populations_excluded_from_original_scope, payer,
    powerless, generational, trapped, continental).

% Women, unfree laborers, and non-landholders were excluded from '39 liberi homines' as originally drafted. The universal reading now retrofits them into the protected class, which benefits their present-day legal claims but obscures the centuries during which the same text was read narrowly to justify their exclusion — the same document served both functions at different times without changing a word.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, historically_marginalized_groups_retrofitted_into_free_men, payer,
    powerless, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__universal_rights_reading, historically_marginalized_groups_retrofitted_into_free_men, beneficiary).

% The sibling reading that would restrict Clause 39's protected class to landowning barons who negotiated the charter as a feudal contract with King John. This reading is not represented in the universal-rights framing's own operation; it exists as a competing constraint story, not a participant here.
narrative_ontology:constraint_stakeholder(magna_carta_1215__universal_rights_reading, baronial_privilege_reading, excluded,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(magna_carta_1215__universal_rights_reading, baronial_privilege_reading).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__universal_rights_reading, human_rights_advocacy_organizations).
narrative_ontology:fixing_cost_class(magna_carta_1215__universal_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, citable anchor for the principle that state power over individuals must be procedurally bounded — a single transhistorical reference point that different legal systems, courts, and advocacy movements can converge on without renegotiating first principles each time.
% TRANSFER_FUNCTION: Moves rhetorical and precedential authority from the specific 1215 baronial settlement to contemporary due-process claimants generally; moves legitimacy from courts' present-day policy judgments to an ostensibly ancient and therefore less contestable source.
% ABSENT_VOICES: The barons who actually negotiated the charter, and King John's crown, are absent — the universal reading depoliticizes their specific feudal bargain into a species of natural law they would not have recognized. The colonized and marginalized groups the reading claims to always-already protect were not consulted in the reading's original construction and remain excluded from meaningfully authoring its application.
% DISAPPEARANCE_RATIONALE: If the universal-rights reading of Clause 39 vanished, due process doctrine would not disappear — it rests on many other textual and philosophical foundations (natural law theory, later constitutional text, international covenants) — but a specific rhetorical resource used in litigation, civic education, and legitimation of judicial review would lose one of its most citable anchors. Whether the world 'rearranges' or stays the same depends on whether one credits the reading with independent causal force or views it as decorative pedigree layered onto conclusions reached on other grounds — hence contested rather than settled.
% FOUNDING_PROBLEM: The clause was drafted to solve a 1215 baronial problem: arbitrary seizure of persons and property by King John without judgment of peers or law of the land, threatening baronial holdings and status specifically.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the human rights advocacy tradition (e.g. scholars of medieval feudal law) attest that the founding problem was narrowly baronial and was resolved or superseded by the settlement itself within years; they corroborate that the 'universal' framing is a later interpretive construction, not the original founding problem's natural extension. No historical source contemporaneous with 1215 corroborates a universal reading; the corroboration for 'still live, universally' comes primarily from the beneficiary tradition itself (jurists and advocates who invoke the clause), which is a caveat on this status answer, not a resolution of it.
narrative_ontology:disappearance_verdict(magna_carta_1215__universal_rights_reading, contested).
narrative_ontology:founding_problem_status(magna_carta_1215__universal_rights_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__universal_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__universal_rights_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__universal_rights_reading_tests).
:- end_tests(magna_carta_1215__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the universal-rights reading's chief function is legitimation-transfer rather than direct material extraction: it moves argumentative authority from present-day policy judgments to an ostensibly ancient, less-contestable textual source, and that transfer has real winners (litigants and advocacy organizations whose claims gain rhetorical weight) and real losers (detained persons and colonized populations for whom the universal promise was historically unenforced or selectively denied, and whose invocation of the reading did not reliably yield protection). Theater ratio rises over the measured interval (0.10 to 0.45) because as the reading became more institutionally entrenched — cited in constitutions, in international instruments, in civic education — an increasing share of its invocation became performative affirmation of shared values rather than operative constraint on state action in the moments protection was most needed (wartime detention, colonial administration, counterterrorism). Suppression (0.38) reflects that dissenting historical readings (that Clause 39 was narrowly baronial) are not coercively silenced but are marginalized in legal education and popular civic narrative, which functions as a softer form of alternative-suppression than outright coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   Human rights advocacy organizations and common law judiciaries sit closest to the beneficiary end: they administer and benefit from the reading's continued authority, and their institutional legitimacy is partly built on the claim of unbroken 800-year descent. Detained persons under executive power and colonized populations sit at the target end: they are the ones for whom the universal promise is invoked as though it protects them, while the actual enforcement of that protection has been most reliably absent exactly when they needed it — a directionality inversion where the group nominally centered by the reading is the group least served by its operation in crisis moments. Historically marginalized groups occupy a genuinely dual position: real present-day beneficiaries of retrofitted inclusion, but also bearing the cost of a centuries-long narrative that obscured their original exclusion.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a specific 1215 baronial dispute over arbitrary seizure by the crown) is largely dead as a live problem — no barons today negotiate with a king over relief payments and scutage. The universal-rights reading persists by claiming the founding problem was never narrowly baronial at all but was always a general due-process principle merely first articulated in a feudal idiom. This is precisely the mandatrophy risk the six-questions genealogy interview is built to surface: founding_problem_status is authored contested rather than dead, because whether the 'true' founding problem was baronial-specific or general-and-timeless is exactly what is under dispute between this reading and its baronial-privilege sibling. The corroboration field notes that medieval legal historians outside the advocacy tradition tend to support the narrower, baronial, resolved-and-superseded reading — which is a genealogical check the universal reading's own proponents cannot self-certify.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_scope_vs_retrofitted_universality,
    'Did Clause 39''s drafters and its immediate medieval interpreters intend or understand ''liber homo'' to name a bounded feudal class, or does the universal-rights reading correctly identify a latent general principle that later interpreters made explicit?',
    'Comparative analysis of contemporaneous 13th-century legal commentary, the 1215 charter''s negotiating history, and the documented scope of who could actually invoke Clause 39 in medieval courts (which excluded villeins, women, and non-freeholders) versus who invokes the universal reading today.',
    'If the original scope was narrowly baronial, the universal-rights reading is a later constructed extension retroactively projected onto the text — this would sharpen the mandatrophy diagnosis and support classifying the reading''s transhistorical claim as largely rhetorical scaffolding rather than continuous precedent. If a genuine latent general principle can be documented in period sources, the universal reading''s claim to continuity strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_scope_vs_retrofitted_universality, empirical, 'Whether medieval sources support universal or narrowly baronial original scope.').

omega_variable(
    committer_structure_which_reading_is_load_bearing,
    'This constraint is one reading (universal_rights_reading) of the magna_carta_1215 kernel, alongside baronial_privilege_reading and living_document_reading. Which reading actually does the load-bearing work in contemporary due-process jurisprudence — is the universal reading cited as an independent authority, or does it function as decorative pedigree layered onto conclusions that living_document_reading''s interpretive-accumulation logic actually produces?',
    'Citation analysis of due-process case law to determine whether courts treat Clause 39 as doing independent argumentative work or as rhetorical ornament following conclusions reached via later constitutional text and precedent (which would indicate the living_document_reading, not this reading, is the operative kernel-reading in practice).',
    'If citation analysis shows Clause 39 functions mainly as ornament, this reading''s practical extraction (its capacity to actually confer or withhold legitimacy) is lower than the authored ε suggests, and the living_document_reading absorbs more of the kernel''s actual operative force. If courts show independent reliance on the universal-rights framing to reach outcomes they could not otherwise justify, this reading''s ε is validated or should rise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_structure_which_reading_is_load_bearing, conceptual, 'Where the kernel''s actual interpretive load is located among sibling readings.').

omega_variable(
    enforcement_gap_as_extraction_signal,
    'Is the historically documented pattern of suspending Clause 39-derived protections during crises (wartime internment, colonial administration, counterterrorism detention) evidence that the universal reading was always aspirational rather than operative, or evidence of a distinct failure to honor an otherwise sound universal principle?',
    'Historical case study comparison across multiple jurisdictions and crisis periods, examining whether suspension of due-process protection correlates with periods when the universal reading was weakly institutionalized versus periods when it was strongly asserted rhetorically but not enforced.',
    'If suspension correlates with weak institutionalization, the reading has genuine but incomplete coordination function. If suspension occurs even during periods of strong rhetorical assertion, the gap between rhetoric and enforcement is itself evidence the universal reading functions partly as legitimation theater — supporting the rising theater_ratio trajectory authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_as_extraction_signal, empirical, 'Whether crisis-period suspensions indicate weak institutionalization or structural theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__universal_rights_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__universal_rights_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1628, magna_carta_1215__universal_rights_reading, theater_ratio, 1628, 0.2).
narrative_ontology:measurement(magn_tr_t1789, magna_carta_1215__universal_rights_reading, theater_ratio, 1789, 0.28).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_1215__universal_rights_reading, theater_ratio, 1900, 0.33).
narrative_ontology:measurement(magn_tr_t1948, magna_carta_1215__universal_rights_reading, theater_ratio, 1948, 0.38).
narrative_ontology:measurement(magn_tr_t2001, magna_carta_1215__universal_rights_reading, theater_ratio, 2001, 0.42).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__universal_rights_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__universal_rights_reading, base_extractiveness, 1215, 0.15).
narrative_ontology:measurement(magn_be_t1628, magna_carta_1215__universal_rights_reading, base_extractiveness, 1628, 0.25).
narrative_ontology:measurement(magn_be_t1789, magna_carta_1215__universal_rights_reading, base_extractiveness, 1789, 0.3).
narrative_ontology:measurement(magn_be_t1900, magna_carta_1215__universal_rights_reading, base_extractiveness, 1900, 0.35).
narrative_ontology:measurement(magn_be_t1948, magna_carta_1215__universal_rights_reading, base_extractiveness, 1948, 0.38).
narrative_ontology:measurement(magn_be_t2001, magna_carta_1215__universal_rights_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__universal_rights_reading, base_extractiveness, 2024, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(magna_carta_1215__universal_rights_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__universal_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__universal_rights_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215_baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__universal_rights_reading, magna_carta_1215_living_document_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraint stories decomposing the natural-language label 'Magna Carta as rights precedent' per the ε-invariance principle. baronial_privilege_reading authors a narrow, historically bounded ε for the original feudal-contract scope. living_document_reading authors ε for the process of interpretive accumulation itself, agnostic to original scope. This story (universal_rights_reading) authors ε for the specific claim that Clause 39 always-already protected all persons. All three share the kernel_id magna_carta_1215 and must remain linked via affects_constraints; none averages over or borrows structural data from the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
