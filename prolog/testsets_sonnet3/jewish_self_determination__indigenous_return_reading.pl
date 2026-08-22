% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Indigenous-Return Reading of Jewish Self-Determination (Zionism as Decolonization)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   This story authors ONE reading — the indigenous-return reading — of a
 *   contested kernel about Jewish self-determination and the character of
 *   Zionism. The reading holds that unbroken religious, linguistic, and
 *   ancestral continuity establishes Jewish indigeneity to the land, and that
 *   this indigeneity reclassifies the Zionist project as decolonization
 *   (restoration of an indigenous people to sovereignty) rather than
 *   colonization (an outside power's settlement of others' land). This is
 *   generated as a clean, self-contained constraint: it does not describe or
 *   average over the sibling readings (liberal_nationalist, settler_colonial,
 *   religious_covenant, diasporist), which are separate constraints with
 *   their own ε, beneficiaries, and classifications, linked here only via
 *   network edges and omega variables per the committer-frame rules.
 *
 * KEY AGENTS:
 *   - jewish_claimants_to_ancestral_land: primary beneficiary of the framing — the indigeneity claim underwrites legitimacy and grounds return/sovereignty arguments
 *   - israeli_state_institutions: agenda-setting beneficiary — institutionalizes the reading in law, education, and diplomacy
 *   - palestinian_communities: excluded from the reading's own adjudication of indigeneity — reframed rather than consulted
 *   - international_law_and_advocacy_bodies: excluded arbiters whose entire framework (settler-colonial analysis) is directly contested by this reading
 *   - comparative_historians_and_archaeologists: analytical observers assessing contested continuity evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.68).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.55).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Indigenous-Return Reading of Jewish Self-Determination (Zionism as Decolonization)").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, '70904cd2-17fa-4492-aef0-2150ebab64bd').
narrative_ontology:cs_kernel_codification('70904cd2-17fa-4492-aef0-2150ebab64bd', distributed).
narrative_ontology:cs_authority_grounding('70904cd2-17fa-4492-aef0-2150ebab64bd', distributed).
narrative_ontology:cs_reading_relation('70904cd2-17fa-4492-aef0-2150ebab64bd', jewish_self_determination__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('70904cd2-17fa-4492-aef0-2150ebab64bd', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('70904cd2-17fa-4492-aef0-2150ebab64bd', jewish_self_determination__religious_covenant_reading, influences).
narrative_ontology:cs_reading_relation('70904cd2-17fa-4492-aef0-2150ebab64bd', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('70904cd2-17fa-4492-aef0-2150ebab64bd', foundational, unbroken_indigenous_continuity_grounds_sovereignty_claim).
narrative_ontology:cs_axiom_status(unbroken_indigenous_continuity_grounds_sovereignty_claim, holdable).
narrative_ontology:cs_axiom_grounding('70904cd2-17fa-4492-aef0-2150ebab64bd', unbroken_indigenous_continuity_grounds_sovereignty_claim, empirically_contingent).
narrative_ontology:cs_axiom('70904cd2-17fa-4492-aef0-2150ebab64bd', foundational, return_of_indigenous_people_cannot_constitute_colonization).
narrative_ontology:cs_axiom_status(return_of_indigenous_people_cannot_constitute_colonization, holdable).
narrative_ontology:cs_axiom_grounding('70904cd2-17fa-4492-aef0-2150ebab64bd', return_of_indigenous_people_cannot_constitute_colonization, conventional).
narrative_ontology:cs_reference_frame('70904cd2-17fa-4492-aef0-2150ebab64bd', biblical_and_second_temple_era_sovereignty).
narrative_ontology:cs_drift_state('70904cd2-17fa-4492-aef0-2150ebab64bd', post_1948_state_formation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('70904cd2-17fa-4492-aef0-2150ebab64bd', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, unbroken_indigenous_continuity_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, decolonization_not_colonization_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold that continuous religious, linguistic, and archaeological ties to the land since antiquity establish indigeneity, and that two millennia of diaspora, expulsion, and persecution did not sever this status. This reading grounds a claim to return and sovereignty framed as restoration rather than acquisition, and grounds moral and legal arguments used in international forums and domestic legitimation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary,
    organized, civilizational, constrained, national).

% Incorporate the indigenous-return reading into foundational narrative, education curricula, and diplomatic argument, using it to characterize territorial policy, settlement, and law as acts of return rather than conquest. Benefits from the framing's capacity to reposition international law categories (colonialism, occupation) as inapplicable.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, israeli_state_institutions, beneficiary).

% Hold continuous presence, land tenure, and self-identification as indigenous to the same territory across many of the same centuries. Under this reading their claim is reframed as either later arrival, subordinate to Jewish indigeneity, or a co-indigenous claim with lesser standing. This reframing does not appear in the reading's own account as an act against them, but it structurally recodes their dispossession claims as inapplicable to a colonization framework — the reading's proponents do not designate them a victim class, which is itself the point of contest.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_communities, excluded,
    powerless, generational, trapped, national).

% Adjudicate claims using settler-colonial and self-determination frameworks developed in other contexts (South Africa, Algeria, the Americas, Australia). The indigenous-return reading directly contests whether these frameworks apply at all, which determines whether these bodies' entire analytic toolkit is relevant or a category error when applied to this territory.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_law_and_advocacy_bodies, excluded,
    institutional, generational, analytical, global).

% Assess competing continuity claims through textual, genetic, linguistic, and archaeological evidence. Findings are contested and selectively invoked by all sides; the discipline itself has no settled consensus on how continuity of population and continuity of political claim relate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, comparative_historians_and_archaeologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, israeli_state_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared narrative resource that coordinates diaspora Jewish political identity, state-building legitimacy claims, and diplomatic argument around a single genealogical thesis, reducing the need to separately justify each policy or territorial claim on its own terms.
% TRANSFER_FUNCTION: Moves rhetorical and legal legitimacy from frameworks that would classify the state-building project as colonial (and thus subject to decolonization remedies) toward frameworks that classify it as indigenous restoration (exempt from those remedies) — the transfer is in classificatory authority and its downstream legal/political consequences, not a material transfer in the first instance.
% ABSENT_VOICES: Palestinian historians, refugee communities, and their advocacy institutions who hold a competing indigeneity and continuous-presence claim are not party to the framing's own internal adjudication; they appear in the reading only as a reframed category (later arrivals, subordinate co-indigenous claimants), not as co-authors of the standard used to judge indigeneity.
% DISAPPEARANCE_RATIONALE: Proponents hold that if the reading disappeared, the underlying historical continuity would remain true and unaffected — only the political framing would change, so 'nothing real' rearranges. Critics hold that the reading does substantial present-day legitimating work (shielding policy from the colonial/decolonial legal and moral vocabulary), such that its disappearance would materially reopen questions of restitution, framework applicability, and international legal characterization — the world of policy and law would rearrange even if the underlying historical facts did not.
% FOUNDING_PROBLEM: How to characterize the return of a diasporic people to a homeland from which it was displaced by conquest and exile, in a 20th-century international order whose primary vocabulary for territorial acquisition (colonialism, settlement, indigeneity) was built to describe European powers appropriating non-European territories inhabited by peoples with no prior claim to the colonizing metropole.
% FOUNDING_PROBLEM_CORROBORATION: Zionist historiography and Israeli state institutions attest the problem as live and correctly solved by this reading. Independent historians of nationalism (e.g. comparative scholars of settler and diasporic nationalisms outside either camp) attest that the underlying historical question — continuity of population versus continuity of political sovereignty — remains genuinely unresolved in the discipline, and that the decolonization/colonization vocabulary itself may be a poor analytic fit for a case involving both ancient origin claims and a modern settlement process; no historian outside the reading's own advocacy network affirms the reading's conclusion (indigenous status therefore decolonization) as settled.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.68) at the story's endpoint because the reading is heavily contested by the referent standard set out in the prompt: 'very low if accepted, but contested classification raises epsilon to high.' Per the ε-referent rule for kernel-reading stories, ε is authored for the standing arrangement under contest AS THIS READING SEES IT — the reading sees itself as descriptively true, but the classificatory work it performs (exempting a modern territorial and demographic arrangement from colonial-framework scrutiny) is itself an act with material downstream stakes, and that classificatory extraction is what is measured here, not the underlying historical continuity claim. Suppression (0.55) reflects the structural difficulty of contesting the framing once embedded in state curricula, diplomatic argument, and international legal strategy, without amounting to formal coercion of dissenters. Theater ratio (0.4) reflects that a substantial share of invocation is now rhetorical/legitimating rather than doing fresh historical argumentative work — the underlying evidentiary case was largely made early; later use increasingly re-deploys the same conclusion as settled premise in venues (UN debates, diplomatic communiqués) where its contested status is not re-litigated.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat, the reading is simply an accurate historical claim being properly applied — no extraction occurs because a true description of ancestry cannot be extractive. From the excluded Palestinian seat and from international law bodies whose framework is being displaced, the same reading operates as a move that pre-empts an entire remedial vocabulary (restitution, decolonization claims, indigenous rights frameworks) without their participation in setting the indigeneity standard. The engine's per-seat computation is expected to diverge sharply between the beneficiary/agenda-setter seats (who would compute something closer to rope or even mountain-adjacent) and the excluded seats (who would compute something closer to tangled_rope or snare) — that divergence is the intended measurement, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants and Israeli state institutions are coded as beneficiaries because the reading directly underwrites legitimacy claims that serve their political and legal interests; their exit options are constrained/arbitrage respectively because the framing is now deeply embedded in institutional and diplomatic practice, not because they face extraction. Palestinian communities are excluded rather than declared victims in base_properties.victims, per the expected structural delta ('victim: none — Palestinian presence reframed as later arrival or co-indigenous with subordinate claim') — this is a structural feature of THIS reading's own self-conception, not an empirical claim being endorsed by the story. Declaring them 'excluded' rather than 'payer' captures that the reading itself does not recognize a victim class; the omega variables carry the question of whether that non-recognition is itself doing extractive work.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to characterize a diasporic people's return to an ancestral homeland within a 20th-century vocabulary built for European colonial contexts) may have been genuinely live in 1897–1948 when no adequate international legal vocabulary existed for this configuration. Whether it remains live in 2024, after 76+ years of a functioning sovereign state, or has been repurposed primarily to pre-empt a specific legal consequence (decolonization remedies), is exactly the founding_problem_status contest recorded above — corroborated from outside the beneficiary set only partially, and only on the narrower point that the historical continuity question is unresolved, not on the reading's further inference to decolonization status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_standard_selection,
    'Which standard of ''indigeneity'' is being applied, and was that standard selected because it is the correct general-purpose standard, or because it is the standard that happens to classify this specific claimant as indigenous while classifying the competing claimant as non-indigenous or subordinate?',
    'Compare the standard used here against standards applied by the same reading''s proponents (or by international law generally) in unrelated indigeneity disputes (e.g. settler-descendant claims elsewhere, diaspora-return claims by other peoples) to test for consistency of application.',
    'If the standard is applied consistently across unrelated cases, the reading''s classification is on stronger structural footing (lower ε warranted). If the standard is bespoke to this case, the reading functions primarily as a legitimating instrument rather than a general theory of indigeneity, and higher ε and a tangled_rope/snare computation from the excluded seats is warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigeneity_standard_selection, conceptual, 'Whether the indigeneity test is general or case-selected.').

omega_variable(
    colonization_decolonization_category_fit,
    'Is the colonization/decolonization binary itself the right analytic category for a case combining ancient-origin claims, millennia of exile, and a 19th–20th century settlement process — or does forcing the case into that binary (in either direction) misdescribe a genuinely novel configuration?',
    'Comparative historical and legal scholarship examining whether other cases of diasporic return to ancestral territory after long absence (if any exist at comparable scale) were usefully analyzed via colonial/decolonial frameworks, versus requiring a distinct category.',
    'If the binary is a poor fit generally, then BOTH this reading and the settler_colonial_reading may be over-claiming by forcing a binary answer, and the contested-classification component of ε (rather than the underlying continuity claim) would be reduced by acknowledging category inadequacy rather than resolving it in either direction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(colonization_decolonization_category_fit, conceptual, 'Whether the colonial/decolonial vocabulary fits this case at all.').

omega_variable(
    coexistence_vs_hierarchy_of_indigeneity,
    'Even granting Jewish indigenous continuity, does establishing that status logically require subordinating or negating a concurrent Palestinian indigeneity claim, or can two populations hold co-indigenous status with neither claim canceling the other?',
    'Examination of comparative cases of co-indigenous or overlapping-indigenous populations elsewhere and how legal/political systems have handled dual indigeneity without forced hierarchy.',
    'If co-indigeneity without hierarchy is coherent, the reading''s move to reframe Palestinian presence as subordinate is a separate and contestable step beyond the continuity claim itself, and the ε attributable to that additional step (rather than to the continuity claim) should be isolated and may be substantially higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_vs_hierarchy_of_indigeneity, conceptual, 'Whether Jewish indigeneity logically requires negating Palestinian indigeneity.').

omega_variable(
    genealogical_continuity_evidentiary_status,
    'What is the actual evidentiary strength of unbroken genealogical/cultural continuity claims (as opposed to religious, textual, and self-identification continuity, which are less contested)?',
    'Independent genetic, archaeological, and historical-demographic research assessed by scholars with no stake in either reading''s political conclusion.',
    'Stronger continuity evidence supports lower ε for the core historical claim; weaker or more mixed evidence (population admixture, conversion history, discontinuous political sovereignty) would not refute Jewish self-identification but would weaken the specific inferential chain from ''continuity'' to ''exemption from colonial-framework analysis.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genealogical_continuity_evidentiary_status, empirical, 'Strength of the underlying genealogical continuity evidence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__indigenous_return_reading, theater_ratio, 1897, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1897, observed).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__indigenous_return_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__indigenous_return_reading, theater_ratio, 1967, 0.28).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__indigenous_return_reading, theater_ratio, 1993, 0.32).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2010, jewish_self_determination__indigenous_return_reading, theater_ratio, 2010, 0.37).
narrative_ontology:measurement_basis(jewi_tr_t2010, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__indigenous_return_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1897, 0.2).
narrative_ontology:measurement_basis(jewi_be_t1897, observed).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.35).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.5).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1993, 0.55).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2010, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement_basis(jewi_be_t2010, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.68).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__indigenous_return_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints instantiating the jewish_self_determination kernel. Each reading authors its own ε, beneficiary/victim structure, and claimed type from its own internal logic — per the ε-invariance principle, they are not five measurements of one constraint but five distinct constraints sharing a contested kernel. The indigenous_return_reading and settler_colonial_reading are the most structurally opposed pair (declared forecloses in cs_structure.reading_relations): the core premise of one (Jewish indigenous return exempts the project from colonial classification) directly negates the core premise of the other (the project is paradigmatically colonial dispossession) within any single adjudicative framework. The liberal_nationalist_reading is compatible with but does not require the indigeneity premise (coexists_with). The religious_covenant_reading and diasporist_reading are affected by (influences) this reading's success or failure in ways that shift their own legitimacy conditions and audience, without being logically foreclosed by it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
