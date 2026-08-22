% ============================================================================
% CONSTRAINT STORY: woman_female_category__hybrid_contextual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__hybrid_contextual_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: woman_female_category__hybrid_contextual_reading
 *   human_readable: Hybrid Contextual Category Membership Rule (sex for medical/sports/safety; identity for social/legal recognition)
 *   domain: political philosophy/bioethics/gender studies/law
 *
 * SUMMARY:
 *   This story instantiates the hybrid contextual reading of the contested
 *   woman/female category kernel: category membership is governed by
 *   biological sex in medical, sports and safety contexts, and by gender
 *   identity in social and legal recognition contexts. The colloquial
 *   question of what makes someone a woman decomposes, per the
 *   epsilon-invariance principle, into three structurally distinct
 *   classification rules — this domain-partition rule, the uniform sex
 *   criterion, and the uniform identity criterion — authored as separate
 *   constraints with separate epsilon values and linked by network edges. The
 *   partition emerged in the 2010s as institutional compromise: courts,
 *   sports bodies and medical regulators facing contradictory obligations
 *   adopted the domain split rather than adjudicate the underlying dispute.
 *   Its structural signature: administering institutions collect the
 *   conflict-minimization dividend; both camps' constituencies bear costs in
 *   the domains where their reading is subordinated; intersex people bear
 *   costs under both criteria. The claimed type and the metrics are authored
 *   independently: I claim tangled_rope because the rule has both a genuine
 *   coordination function and asymmetric extraction; the metrics describe
 *   moderate, slowly rising extraction with rising theater and enforcement
 *   load.
 *
 * KEY AGENTS:
 *   - domain_administering_institutions: Agenda-setter and primary beneficiary (institutional/arbitrage) — sets which criterion governs each domain, enforces the partition, collects the conflict-minimization dividend
 *   - trans_women: Primary target in sex-governed domains (moderate/identity_locked) — categorized male where sex governs, recognized where identity governs
 *   - sex_based_provision_claimants: Primary target in identity-governed domains (organized/constrained) — sex-based claims overridden where identity governs
 *   - intersex_individuals: Target under both criteria (moderate/trapped) — fit neither binary, categorized by whichever rule each domain uses
 *   - trans_men: Target in sex-governed domains (moderate/identity_locked) — categorized female where sex governs
 *   - affected_service_users: Excluded seat (powerless/trapped) — the partition is applied to them without their participation
 *   - courts_and_human_rights_bodies: Analytical observer (institutional/analytical) — adjudicate the partition's boundaries jurisdiction by jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, 0.55).
domain_priors:suppression_score(woman_female_category__hybrid_contextual_reading, 0.6).
domain_priors:theater_ratio(woman_female_category__hybrid_contextual_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(woman_female_category__hybrid_contextual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__hybrid_contextual_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__hybrid_contextual_reading, "Hybrid Contextual Category Membership Rule (sex for medical/sports/safety; identity for social/legal recognition)").
narrative_ontology:topic_domain(woman_female_category__hybrid_contextual_reading, "political philosophy/bioethics/gender studies/law").

domain_priors:requires_active_enforcement(woman_female_category__hybrid_contextual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__hybrid_contextual_reading, '73a14277-b108-4560-973e-e59fd0e81046').
narrative_ontology:cs_kernel_codification('73a14277-b108-4560-973e-e59fd0e81046', distributed).
narrative_ontology:cs_authority_grounding('73a14277-b108-4560-973e-e59fd0e81046', distributed).
narrative_ontology:cs_reading_relation('73a14277-b108-4560-973e-e59fd0e81046', woman_female_category__sex_biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('73a14277-b108-4560-973e-e59fd0e81046', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_axiom('73a14277-b108-4560-973e-e59fd0e81046', foundational, domain_function_determines_criterion).
narrative_ontology:cs_axiom_status(domain_function_determines_criterion, holdable).
narrative_ontology:cs_axiom_grounding('73a14277-b108-4560-973e-e59fd0e81046', domain_function_determines_criterion, instrumental).
narrative_ontology:cs_axiom('73a14277-b108-4560-973e-e59fd0e81046', secondary, no_single_criterion_suffices).
narrative_ontology:cs_axiom_status(no_single_criterion_suffices, holdable).
narrative_ontology:cs_axiom_grounding('73a14277-b108-4560-973e-e59fd0e81046', no_single_criterion_suffices, empirically_contingent).
narrative_ontology:cs_reference_frame('73a14277-b108-4560-973e-e59fd0e81046', domain_partitioned_category_membership).
narrative_ontology:cs_drift_state('73a14277-b108-4560-973e-e59fd0e81046', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('73a14277-b108-4560-973e-e59fd0e81046', '').
narrative_ontology:cs_kernel_id(woman_female_category__hybrid_contextual_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__hybrid_contextual_reading, domain_administering_institutions).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, sex_based_provision_claimants).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, intersex_individuals).
narrative_ontology:constraint_victim(woman_female_category__hybrid_contextual_reading, trans_men).
narrative_ontology:constraint_vindicates(woman_female_category__hybrid_contextual_reading, contextual_classification_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sports federations, medical regulators, legislatures, custodial authorities and service agencies each decide which membership criterion governs their domain: biological sex for athletic eligibility, clinical protocols and safety provisions; gender identity for documents, anti-discrimination protection and social recognition. They publish the policies, run the eligibility panels, and defend the partition in court when either camp challenges it. They can re-partition at policy-review cycles, and the calm the partition purchases accrues to them as operational simplicity and legitimacy.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, domain_administering_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__hybrid_contextual_reading, domain_administering_institutions, beneficiary).

% Are categorized as male wherever the sex criterion governs: excluded from women's competitive categories under most elite sports frameworks, routed to male-default clinical protocols in some medical contexts, and denied some sex-specific safety accommodations. Are recognized as women wherever the identity criterion governs: documents, anti-discrimination law, social life. Their stake in the category is constitutive — there is no exit from being categorized — and the domains where the sex criterion governs are precisely where their recognition is most contested.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_women, payer,
    moderate, biographical, identity_locked, global).

% Women and advocacy organizations whose claims to single-sex spaces, services, sports categories and sex-disaggregated data presuppose that the category tracks biological sex. Wherever the identity criterion governs, those claims are overridden: refuges, prisons and services operating identity-based admission cannot maintain sex-based rules, and records that capture gender identity cannot answer sex-specific questions. They retain footholds where the sex criterion governs and litigate to hold them, but cannot exit the category whose membership rule is in dispute.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, sex_based_provision_claimants, payer,
    organized, generational, constrained, global).

% People whose chromosomes, anatomy or development fit neither binary criterion cleanly. Under the sex criterion they are assigned by administrative or historical surgical decision; under the identity criterion they are assigned by self-declaration that may match neither. The partition gives them no domain in which their specific situation is what the criterion tracks; they are categorized under whichever rule each domain uses, often by decisions made in childhood without their consent.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, intersex_individuals, payer,
    moderate, biographical, trapped, global).

% Are categorized as female wherever the sex criterion governs: required to compete in women's categories absent medical thresholds in some frameworks, and included in clinical services framed by sex (cervical screening, reproductive care). Are recognized as men wherever the identity criterion governs. Their stake mirrors trans women's: the domains where the sex criterion governs are where the categorization conflicts most directly with their lives.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, trans_men, payer,
    moderate, biographical, identity_locked, global).

% Patients in sex-specific protocols, women in refuges and prisons, trans athletes and clinic patients — the people the partition is applied to in concrete situations. Rule-setting bodies include the advocacy organizations that speak for each camp, but the individuals living under the resulting rules rarely hold seats on eligibility panels, policy consultations or standard-setting committees.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, affected_service_users, excluded,
    powerless, biographical, trapped, global).

% Adjudicate which criterion governs which domain when the partition is challenged: equality-law cases, sports arbitration awards, human-rights rulings. Their decisions move the partition's boundaries jurisdiction by jurisdiction, and their reasoning documents the contradictory obligations that produced the partition in the first place.
narrative_ontology:constraint_stakeholder(woman_female_category__hybrid_contextual_reading, courts_and_human_rights_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__hybrid_contextual_reading, domain_administering_institutions).
narrative_ontology:fixing_cost_class(woman_female_category__hybrid_contextual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutions across medicine, sport, law, safety and social administration must assign category membership consistently while the population disagrees about the membership criterion. The hybrid rule assigns each domain the criterion its function most directly requires — biological sex where physical bodies are the operative fact (clinical protocols, competitive fairness, safety provisions), gender identity where social and legal recognition is the operative fact — so that no institution must adjudicate the underlying dispute in order to operate.
% TRANSFER_FUNCTION: Moves categorization costs from administering institutions to the categorized: in sex-governed domains, category access and accommodations are withheld from trans women and trans men; in identity-governed domains, sex-based claims to spaces, services and data are overridden for sex-based provision claimants; intersex people bear assignment costs under both criteria. What flows to the institutions is the absence of conflict — operational simplicity, legitimacy, and freedom from resolving the underlying question.
% ABSENT_VOICES: The people the partition is applied to — patients, prisoners, athletes, service users — reach rule-setting tables only through advocacy organizations that speak for one camp or the other; individuals whose situations cut across the camps (intersex people, detransitioners, gender-nonconforming people who fit neither camp's framing) hold no seat. Both camps' organizations are in the room; the categorized are largely not.
% DISAPPEARANCE_RATIONALE: If the partition vanished overnight, every administering institution would have to adopt a uniform criterion immediately: sports bodies would face eligibility chaos, medical systems protocol conflicts, legal systems recognition conflicts — and whichever criterion each picked would trigger maximal political conflict with the losing camp. The current allocation of access and cost across sport, medicine, law and services is organized around the partition and would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: In the 2010s, expanding legal gender recognition collided with sex-based eligibility and safety rules in sport, medicine and custodial settings. Courts and agencies faced contradictory obligations — anti-discrimination frameworks recognizing gender identity, fairness and safety rules requiring sex categories — and risked paralysis or arbitrary case-by-case decisions. The hybrid partition was formalized to give each domain a workable criterion without resolving the underlying dispute.
% FOUNDING_PROBLEM_CORROBORATION: Judicial opinions and sports arbitration awards from multiple jurisdictions document the contradictory obligations in their own reasoning; legislative consultation records and academic legal scholarship across the political spectrum attest that institutions faced incompatible demands. Courts sit outside the sports bodies and medical regulators that directly collect the partition's benefits, though they share its institutional world. No source entirely outside the contest attests the founding problem, because the problem is constituted by the contest itself — but the documentary record of institutional paralysis predates and does not depend on either camp's advocacy.
narrative_ontology:disappearance_verdict(woman_female_category__hybrid_contextual_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__hybrid_contextual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__hybrid_contextual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__hybrid_contextual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__hybrid_contextual_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__hybrid_contextual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__hybrid_contextual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__hybrid_contextual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.55) because the partition imposes real, recurring costs on both camps in their subordinated domains while also delivering classification both camps partially endorse — each camp's criterion governs the domains that camp prioritizes, which is what makes this a hybrid rather than a snare. Suppression (0.6) is structural: eligibility panels, recognition frameworks and service rules enforce the partition; there is no violent enforcement, but the categorized cannot exit and the uniform alternatives are held off the table by institutional non-adoption rather than by argument. Suppression is authored as a raw structural property, unscaled; only extractiveness is scaled downstream by directionality and scope. Theater (0.4) reflects the gap between the principled context-sensitivity framing and the partition's actual function as conflict management — the functional content is real, but a growing share of institutional activity defends the framing rather than the classification. Accessibility collapse is low (0.3): both uniform readings remain fully articulated, litigated and advocated; the partition suppresses them by non-adoption, not foreclosure. Resistance is high (0.7): both camps actively contest the partition, and jurisdictional divergence is its main drift vector. The measurement series share one time grid; suppression_requirement is authored because the story tracks enforcement-capacity change — eligibility panels, review processes and litigation defense were built up over the interval, so enforcement hardening is part of the narrative, not just a static scalar.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the victim seats compute differently by construction. From the administering institutions' position the partition is prudent governance: every domain gets a workable criterion, both camps win somewhere, conflict is contained. From either victim seat the same structure is subordination in the domain that matters most — and each victim seat experiences the OTHER domain's criterion as the imposition: trans women experience sex-governed domains as exclusion; sex-based provision claimants experience identity-governed domains as erasure of the category's basis. The institution sees symmetry (each camp wins somewhere); the victims each see asymmetry (I lose where it counts). A further structural feature: the two victim groups cannot readily form a coalition, because each camp's preferred fix — its own uniform criterion — would intensify the other's subordination. The partition's extraction is partly protected by the victims' opposition to each other, which the resistance metric reflects but cannot fully capture.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration maps to the institutions that collect the dividend without bearing categorization costs, with arbitrage-grade exit (they can re-partition at policy cycles) — directionality near the beneficiary end. Trans women and sex-based provision claimants are declared victims: each bears the partition's costs in their subordinated domains, and those domains are where category membership is most load-bearing for them, so their effective extraction sits near the target end despite partial benefit in the other domains. Intersex individuals are victims under both criteria with no domain where the criterion fits them — nearest the full-target end. Trans men mirror trans women in sex-governed domains. The domain-shifting structure means each victim's directionality is a weighted average across domains; the weights fall where the category is constitutive for that group, which is what keeps net extraction on the victims rather than diffusing it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the partition as pure coordination (rope) would erase the extraction both camps suffer where subordinated and the intersex costs under both criteria. Reading it as pure extraction (snare) would erase the genuine coordination function — institutions facing contradictory obligations really do need a workable partition, and the rule delivers domain-appropriate classification that both camps partially endorse. Tangled rope holds both. On mandatrophy: the founding problem (institutional paralysis under contradictory obligations) is still live, so the mandate has not outlived its function. But the mandate is parasitic on the underlying disagreement: if one camp's reading wins culturally, the partition's mandate dies and the rule would persist only as institutional inertia — decaying toward a piton (defended theatrically, benefiting no one enough to maintain it) or being replaced by the winning sibling's uniform rule. The partition_stability omega tracks exactly this trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the woman_female_category kernel. How would its structure change under the sibling readings, and what does this reading''s domain-shifting victim set depend on?',
    'Compare the compiled structure against the sibling stories (woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading): under the uniform sex reading the victim set collapses to trans and intersex people in every domain; under the uniform identity reading it collapses to sex-based provision claimants in every domain.',
    'The hybrid reading''s moderate epsilon and dual victim set are artifacts of the partition; under either sibling the constraint becomes a fixed-victim-set structure whose epsilon concentrates on one group, and the classification and network edges all shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: this story instantiates the hybrid contextual reading; either sibling reading would fix the victim set and relocate epsilon.').

omega_variable(
    partition_stability,
    'Is the domain partition (sex for medical/sports/safety, identity for social/legal) stable, or is it a political compromise that drifts as power shifts between jurisdictions and camps?',
    'Track legislative, judicial and federation-policy changes to which criterion governs which domain across jurisdictions over time; jurisdictional divergence (uniform-sex rulings in some systems, self-identification regimes in others) is the drift signal.',
    'If the partition is drifting apart jurisdictionally, the constraint behaves as a transitional settlement rather than a steady-state rule, and its theater and suppression components rise as defending it grows costlier; if it stabilizes, it hardens into a durable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_stability, empirical, 'Whether the context partition is a durable rule or a drifting compromise.').

omega_variable(
    principled_partition_or_deferral,
    'Is the partition principled — each domain''s function genuinely requires its criterion — or a deferral by which institutions avoid adjudicating the underlying dispute?',
    'Test the partition against functional necessity: domains whose operation is biologically load-bearing (clinical dosing, competitive physiology) versus domains where recognition is the point (documents, anti-discrimination protection). Domains where the assigned criterion is functionally unnecessary indicate deferral.',
    'If largely principled, the constraint sits nearer the coordination end and its theater component is low; if largely deferral, the theater ratio understates the avoidance function and the constraint is closer to pure extraction with a coordination cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principled_partition_or_deferral, conceptual, 'Whether the context-relativity is functional or conflict-avoidant.').

omega_variable(
    boundary_case_stress,
    'Where the two criteria collide within a single domain — intersex athletes, legal recognition versus medical records, custodial assignment — does the partition hold, or does one criterion swallow the other under stress?',
    'Case law and eligibility disputes at the boundary: if adjudication consistently resolves collisions in favor of one criterion, the partition is not stable and the constraint resolves into one of its siblings in practice.',
    'Systematic boundary breakdown would show the hybrid rule is a deferral that resolves under stress rather than a working classification, shifting the effective structure toward the winning sibling''s fixed-victim-set shape.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_case_stress, empirical, 'Whether the partition survives collisions between its two criteria.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__hybrid_contextual_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__hybrid_contextual_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t3, woman_female_category__hybrid_contextual_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(woma_tr_t3, observed).
narrative_ontology:measurement(woma_tr_t6, woman_female_category__hybrid_contextual_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(woma_tr_t6, observed).
narrative_ontology:measurement(woma_tr_t9, woman_female_category__hybrid_contextual_reading, theater_ratio, 9, 0.34).
narrative_ontology:measurement_basis(woma_tr_t9, observed).
narrative_ontology:measurement(woma_tr_t12, woman_female_category__hybrid_contextual_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(woma_tr_t12, observed).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__hybrid_contextual_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(woma_tr_t15, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__hybrid_contextual_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t3, woman_female_category__hybrid_contextual_reading, base_extractiveness, 3, 0.47).
narrative_ontology:measurement_basis(woma_be_t3, observed).
narrative_ontology:measurement(woma_be_t6, woman_female_category__hybrid_contextual_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement_basis(woma_be_t6, observed).
narrative_ontology:measurement(woma_be_t9, woman_female_category__hybrid_contextual_reading, base_extractiveness, 9, 0.51).
narrative_ontology:measurement_basis(woma_be_t9, observed).
narrative_ontology:measurement(woma_be_t12, woman_female_category__hybrid_contextual_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(woma_be_t12, observed).
narrative_ontology:measurement(woma_be_t15, woman_female_category__hybrid_contextual_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(woma_be_t15, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__hybrid_contextual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t3, woman_female_category__hybrid_contextual_reading, suppression_requirement, 3, 0.53).
narrative_ontology:measurement_basis(woma_su_t3, observed).
narrative_ontology:measurement(woma_su_t6, woman_female_category__hybrid_contextual_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement_basis(woma_su_t6, observed).
narrative_ontology:measurement(woma_su_t9, woman_female_category__hybrid_contextual_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement_basis(woma_su_t9, observed).
narrative_ontology:measurement(woma_su_t12, woman_female_category__hybrid_contextual_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(woma_su_t12, observed).
narrative_ontology:measurement(woma_su_t15, woman_female_category__hybrid_contextual_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(woma_su_t15, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__hybrid_contextual_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__hybrid_contextual_reading, woman_female_category__gender_identity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the woman/female category' decomposes into three structurally distinct classification rules per the epsilon-invariance principle — the uniform sex criterion, the uniform identity criterion, and this domain-partition rule. Each has its own epsilon, victim set, and beneficiaries; forcing one story to cover all three would make epsilon observer-relative. The hybrid reading is downstream of both siblings: it cites each sibling's domains as evidence (medicine's biological dependence, law's recognition function), and each sibling camp attacks the partition precisely in the domains it concedes. Edges run from this story to both siblings; the siblings' own stories carry the reciprocal edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
