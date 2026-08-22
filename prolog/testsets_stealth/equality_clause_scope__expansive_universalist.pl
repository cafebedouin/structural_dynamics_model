% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of the Equality Clause
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   The constitutional equality clause — the declaration that all are created
 *   equal and its doctrinal descendants — is a contested kernel with three
 *   live readings. This file instantiates ONE of them, the expansive
 *   universalist reading: the clause states a self-evident universal truth
 *   binding on all humans, historical exclusions (slavery, property
 *   qualifications, the legal subordination of women) are hypocrisy to be
 *   corrected rather than binding precedent, and courts may expand the
 *   clause's applications by interpretation at a low legitimacy threshold.
 *   The arrangement this reading governs is a constitutional order in which
 *   universal equal standing is judicially enforced and expansible. The
 *   sibling readings — equality_clause_scope__restrictive_originalist
 *   (equality confined to propertied white male political actors within the
 *   18th-century social contract) and
 *   equality_clause_scope__progressive_textualist (a textual principle whose
 *   scope expands through democratic amendment rather than judicial
 *   reinterpretation) — are separate constraints with their own epsilon,
 *   beneficiary sets, and types; per the epsilon-invariance rule they are
 *   neither described inside this story nor averaged into it. Claim and
 *   metrics are authored independently: the reading's own rhetoric claims
 *   natural-law status ('self-evident truth'), while the authored metrics
 *   describe a hybrid structure — genuine membership coordination carrying
 *   identifiable displacement costs under active judicial enforcement. The
 *   engine computes each seat's type; where its output diverges from the
 *   claim, that divergence is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - judiciary: agenda_setter (institutional/constrained) — administers the reading; the seat where displaced interpretive authority accrues
 *   - historically_excluded_groups: primary beneficiary (organized/constrained) — their inclusion is the reading's direct product
 *   - all_persons_within_jurisdiction: universal beneficiary class (moderate/constrained) — the reading's signature structural feature
 *   - historical_privilege_holders: primary payer (powerful/constrained) — exclusive standing dissolved by universal application
 *   - democratic_majorities: payer and incidental beneficiary (organized/constrained) — bears the counter-majoritarian displacement
 *   - civil_rights_advocacy_organizations: organized beneficiary (organized/mobile) — litigates the claims the reading makes actionable
 *   - non_citizen_persons_outside_jurisdiction: excluded voice (powerless/constrained, global) — named by the universal claim, unreached by enforcement
 *   - legal_academy: analytical observer (analytical/analytical) — attests the founding gap, supplies the doctrinal arguments every seat borrows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.3).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.35).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.3).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of the Equality Clause").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, '4acdd66d-4aa8-4602-b988-76ddf4eeb6ef').
narrative_ontology:cs_kernel_codification('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', fixed_text).
narrative_ontology:cs_authority_grounding('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', lineage).
narrative_ontology:cs_interpretation_layer_present('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef').
narrative_ontology:cs_reading_relation('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', foundational, equality_self_evident_universal).
narrative_ontology:cs_axiom_status(equality_self_evident_universal, holdable).
narrative_ontology:cs_axiom_grounding('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', equality_self_evident_universal, deontological).
narrative_ontology:cs_axiom('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', foundational, historical_exclusions_non_binding_hypocrisy).
narrative_ontology:cs_axiom_status(historical_exclusions_non_binding_hypocrisy, holdable).
narrative_ontology:cs_axiom_grounding('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', historical_exclusions_non_binding_hypocrisy, deontological).
narrative_ontology:cs_axiom('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', secondary, judicial_expansion_legitimacy_low_threshold).
narrative_ontology:cs_axiom_status(judicial_expansion_legitimacy_low_threshold, holdable).
narrative_ontology:cs_axiom_grounding('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', judicial_expansion_legitimacy_low_threshold, instrumental).
narrative_ontology:cs_reference_frame('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', self_executing_universal_equality).
narrative_ontology:cs_drift_state('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', contemporary_appointment_politics_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4acdd66d-4aa8-4602-b988-76ddf4eeb6ef', '2026-08-10T12:00:00Z').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_excluded_groups).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, all_persons_within_jurisdiction).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, historical_privilege_holders).
narrative_ontology:constraint_victim(equality_clause_scope__expansive_universalist, democratic_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, democratic_majorities).
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, civil_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_equality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the equality clause and sets aside laws that deny persons equal standing. Each expansion ruling extends the court's authority over the clause's meaning; the bench is appointed through the same political process whose outputs it reviews. Its alternative to expanding is judicial restraint, which the institution treats as abdication of its constitutional role.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Persons whom the clause's historical application excluded — enslaved people and their descendants, women, the non-propertied — and their successors. They hold enforceable equal-standing claims only insofar as this reading governs; their inclusion is the reading's direct product. Exit would mean leaving the jurisdiction or accepting the historical application's limits.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_excluded_groups, beneficiary,
    organized, generational, constrained, national).

% Every person subject to the jurisdiction's laws, who hold equal standing under the clause only if the universal reading prevails. The class is diffuse; its members encounter the arrangement mostly through the rights it protects, the disputes it settles, and the school curricula and civic rituals that transmit its premise.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, all_persons_within_jurisdiction, beneficiary,
    moderate, generational, constrained, national).

% The class whose exclusive political and civil standing under the historical application — propertied white male political membership — is dissolved when the reading applies universally. They retain economic and political resources and contest the reading through appointment politics, scholarship, and legislation, but cannot exit the legal order that now includes those their class formerly excluded.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historical_privilege_holders, payer,
    powerful, generational, constrained, national).

% Voters and their legislatures, whose policy choices on contested social questions are set aside when courts find them inconsistent with the universal principle. They also live inside the protection the reading affords — most belong to the class it equalizes — and their recourse is constitutional amendment, appointment politics, or jurisdiction-stripping, each slow and uncertain.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, democratic_majorities, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(equality_clause_scope__expansive_universalist, democratic_majorities, beneficiary).

% Litigating and advocacy organizations that bring the equality claims the reading makes actionable. Their docket, standing, and funding depend on the reading remaining operative; they can and do shift between litigation and legislative strategy, which gives them a genuine alternative most other seats lack.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Persons beyond the jurisdiction's borders whom the reading's universal language — all humans — names but whom its enforcement never reaches. They hold no claim in the arrangement's courts; their recourse is migration, which the jurisdiction's borders price and ration. The gap between the universal promise and the jurisdictional enforcement is the shape of their position.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, non_citizen_persons_outside_jurisdiction, excluded,
    powerless, generational, constrained, global).

% Constitutional scholars and historians who map the clause's interpretive history, document the founding-era gap between declaration and practice, and supply the doctrinal arguments every other seat borrows. They bear none of the arrangement's costs and collect none of its proceeds.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, legal_academy, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__expansive_universalist, judiciary).
narrative_ontology:fixing_cost_class(equality_clause_scope__expansive_universalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles who counts as an equal member of the political community: the reading supplies a single criterion — human equality, indifferent to historical qualification — where the historical practice supplied a patchwork of property, race, and sex qualifications. It also gives courts one uniform standard for evaluating laws across the jurisdiction instead of divergent local membership rules.
% TRANSFER_FUNCTION: Moves decision authority over the clause's scope from legislatures and the historically privileged political class to the courts; moves enforceable equal-standing claims to persons the historical application excluded; dissolves the exclusive standing the privileged class held under the founding settlement.
% ABSENT_VOICES: Persons outside the jurisdiction — named by the reading's universal language but unreachable by its enforcement — would object that 'all humans' stops at the border. Democratic majorities are present in the conversation only as losing litigants at the moment of override. The founding generation's own excluded contemporaries are spoken for by every reading and present in none.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight, the rights landscape built on it — desegregation, sex equality, the reapportionment lineage, the equality components of substantive due process — would lose its doctrinal foundation; exclusionary laws would face revived challenge under the historical application; and interpretive authority over the clause would shift back to legislatures and to whatever historical understanding courts then found binding. The rearrangement would be large, fast, and fought over.
% FOUNDING_PROBLEM: An 18th-century constitution declares equality while practicing exclusion: the founding documents proclaimed universal equal standing under a social contract whose actual membership was propertied, white, and male, with slavery, property qualifications, and the legal subordination of women operating underneath the declaration.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside every reading's beneficiary set document the founding-era gap between declared principle and practiced exclusion. The originalist legal movement — an adverse party — corroborates that the founding framework was restrictive while disputing that this is a defect to be corrected rather than a binding term. International human-rights monitoring bodies attest that formal exclusion within the jurisdiction has been dismantled while flagging enforcement gaps and the jurisdictional limit on the universal claim. No seat's self-attestation is relied on.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).
:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.30, matching the 2026 measurement point: assessed by the reading's own lights, the arrangement's costs are corrections of illegitimate exclusion, but the structure still takes identifiable things from identifiable seats — the privileged class's dissolved standing and the democratic majority's displaced control over rights scope — and a candid universalist concedes the counter-majoritarian displacement while endorsing it. Suppression is authored at 0.35 as a raw structural property (the engine scales only extractiveness, by directionality and scope): courts set aside legislative outputs and enforce compliance — coercion that is bounded, public, and legitimated rather than covert. Theater is low (0.12): the arrangement's activity is overwhelmingly functional adjudication; ceremonial invocation of 'created equal' is real but residual. Accessibility collapse is moderate (0.45): within the courts the historical application has collapsed — no serious contemporary doctrine defends formal exclusion — but in the political culture the restrictive reading persists as a live position pursued through appointments and scholarship, so alternatives are partly, not completely, collapsed. Resistance is substantial (0.6): organized originalist jurisprudence, appointment politics, and periodic court-curbing proposals. All three tracked series run on one shared eight-point grid (1776-2026), every metric authored at every point. The series oscillate rather than drift: enforcement capacity and displacement costs surged with Reconstruction (1868), retreated in the Plessy era (1896), surged again from Brown (1954) through the Warren Court (1968), and have settled into consolidation (2026). The cycle tracks appointment politics and social-movement pressure — it is a side effect of external political cycles, not itself an extraction mechanism, and no intermittent-reinforcement dynamic is claimed. The 2026 scalars are a consolidation-phase reading; the oscillation-stability omega flags that the cycle may reopen.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by structure, not by information. From the judiciary's seat the arrangement is its own accumulated authority — each expansion ruling is an exercise of a power only it holds, and the arrangement looks like the constitution working as designed. From the historically excluded groups' seat it is the instrument of their inclusion, and its costs fall entirely elsewhere. From the historical privilege holders' seat it is the dissolution of standing their class wrote into the founding settlement, enforced by institutions that class once controlled. From the democratic majorities' seat it is the recurring experience of losing votes that courts decline to count on contested social questions — while most of those same majorities live inside protections the arrangement affords them. Same clause, same text, four different arrangements experienced; the engine computes the divergence from the structural data, and the authored claim does not adjudicate it. Coalition potential is real but asymmetric: the majorities' recourse runs through amendment and appointment (slow, supermajoritarian), while the beneficiary seats' coordination runs through litigation the arrangement itself makes actionable.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: historically_excluded_groups — the arrangement's direct product is their inclusion — and all_persons_within_jurisdiction, the universal class the reading equalizes; both derive low d and correspondingly low effective extraction. Victim declarations: historical_privilege_holders, whose exclusive political standing is the thing dissolved, and democratic_majorities, who bear the counter-majoritarian displacement; both derive high d, with the majorities' dual position (they are also inside the protected class, hence the secondary beneficiary role) tempering but not reversing their payment. Two overrides are needed where the derivation has no declaration to read. The judiciary holds no beneficiary or victim declaration, yet it is the arrangement's administrator and the seat where the displaced interpretive authority demonstrably accrues; the canonical fallback would misplace it, so the institutional atom is overridden to d=0.2, near the beneficiary end. Non-citizen persons outside the jurisdiction are excluded rather than targeted: the reading names them and the enforcement never reaches them — a relationship of omission rather than subsidy or extraction; the powerless atom is overridden to d=0.55, near-symmetric with a slight tilt toward cost, reflecting the unkept universal promise at the border. Civil-rights advocacy organizations derive low d as organized beneficiaries with genuine exit (they can shift to legislative strategy). Spatial scope is national for the enforcement, so the engine's scope amplification of extractiveness is moderate; the one global seat sits outside the enforcement's reach entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification discipline matters most here because the reading's own rhetoric is a mountain claim: 'self-evident universal truth' asserts natural-law status, and a mountain classification would absorb the arrangement's displacement costs as correction of physics-like error. Authoring tangled_rope keeps both faces visible: the genuine coordination function — a single criterion of equal membership replacing a patchwork of property, race, and sex qualifications, without which the political community has no shared answer to 'who counts' — and the asymmetric payment that runs through the same interpretive structure. A snare classification would be equally wrong: it would erase the delivered inclusion, the absence of exit-suppression (the amendment channel remains open; the restrictive reading remains publishable and politically actionable), and the fact that enforcement is public and contestable rather than covert. On the founding problem: the gap between declared universality and practiced exclusion is contested rather than dead — formal exclusion within the jurisdiction is dismantled, but the parties dispute whether substantive exclusion persists and whether the universal claim's jurisdictional limit is a live failure. The arrangement therefore cannot be declared mandate-resolved: it still performs work its beneficiaries can name, and its payers still contest both the work and the bill.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the kernel equality_clause_scope. What would change structurally if a sibling reading governed instead?',
    'Comparative classification across the family: author the restrictive_originalist and progressive_textualist readings as separate stories and compare beneficiary sets (universal vs. propertied-white-male vs. amendment-gated), expansion mechanisms (judicial interpretation vs. historical binding vs. democratic amendment), and epsilon values over each reading''s own referent.',
    'Under restrictive_originalism the beneficiary set collapses to a qualified political class and the historically excluded become the victim class; under progressive_textualism the beneficiary set stays universal in principle but expansion authority moves from courts to the amendment process, raising the legitimacy threshold and shifting who pays. This file''s epsilon (0.30), victim set, and type are valid only for the universalist reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-contest routing: this story is one reading; siblings would restructure beneficiaries, enforcement path, and epsilon.').

omega_variable(
    self_evidence_naturalness,
    'Is universal human equality a discovered natural truth the clause merely records, or a constructed interpretive commitment enforced by institutions?',
    'Not resolvable by data alone: the reading''s own rhetoric asserts self-evidence, but the arrangement''s operation is observable — enforcement capacity tracks appointment politics and social-movement pressure, oscillates historically, and stops at jurisdictional borders, none of which resembles a discovery process. The empirical component resolves by whether the principle''s application correlates with enforcement capacity rather than with any independent confirmation.',
    'If treated as discovered natural law, the arrangement''s displacement costs get absorbed as correction of physics-like error and the extraction assessment drops toward zero; if constructed, the costs remain attributable to the structure and the tangled_rope claim stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_evidence_naturalness, conceptual, 'Natural-law vs. constructed-commitment status of the equality principle — the reading claims the former; the operating structure suggests the latter.').

omega_variable(
    counter_majoritarian_cost_attribution,
    'Is the displacement of legislative authority by judicial expansion a genuine cost borne by democratic majorities, or a reallocation they would ratify if they shared the reading''s assessment of the excluded claims?',
    'Behavioral and survey evidence on whether majorities contest the interpretive authority as such or only specific applications; jurisdictional natural experiments where amendment routes replaced judicial routes for comparable expansions.',
    'If majorities ratify the authority, the democratic_majorities victim declaration weakens and epsilon falls; if they contest the authority itself, the counter-majoritarian displacement stands as a real cost and the hybrid asymmetry is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_majoritarian_cost_attribution, empirical, 'Whether the counter-majoritarian displacement is extraction or ratified reallocation.').

omega_variable(
    jurisdictional_universality_gap,
    'The reading names all humans; the arrangement enforces within jurisdiction. Is the border a constitutive limit of the constraint or a contingent enforcement gap its own principle condemns?',
    'Doctrinal analysis of whether courts treat the universal claim as extra-jurisdictional rhetoric or as an operative limit principle; comparative data on extension of equal standing to non-citizens and extraterritorial persons.',
    'If constitutive, the universal claim is partial cover and the excluded non-citizen seat becomes a candidate victim class, raising epsilon; if contingent, the gap is an unforced error the reading''s own lights require correcting and epsilon is unaffected.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jurisdictional_universality_gap, conceptual, 'Whether the arrangement''s jurisdictional limit contradicts its own universal premise.').

omega_variable(
    enforcement_oscillation_stability,
    'Has the enforcement oscillation (Reconstruction surge, Plessy-era retreat, Warren-era surge, contemporary consolidation) converged to a stable plateau, or does the arrangement remain cycle-prone through appointment politics?',
    'Track appointment-driven doctrinal retrenchment and expansion across successive courts; measure whether suppression_requirement re-oscillates after the interval end.',
    'If cycle-prone, the 2026 consolidation values are a phase reading rather than an end state, and the base_properties scalars should be read as mid-cycle; if stable, the scalars describe the arrangement''s steady condition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_oscillation_stability, empirical, 'Whether the historical enforcement cycle is closed or still open.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1776, equality_clause_scope__expansive_universalist, theater_ratio, 1776, 0.3).
narrative_ontology:measurement(equa_tr_t1820, equality_clause_scope__expansive_universalist, theater_ratio, 1820, 0.45).
narrative_ontology:measurement(equa_tr_t1857, equality_clause_scope__expansive_universalist, theater_ratio, 1857, 0.6).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__expansive_universalist, theater_ratio, 1868, 0.2).
narrative_ontology:measurement(equa_tr_t1896, equality_clause_scope__expansive_universalist, theater_ratio, 1896, 0.45).
narrative_ontology:measurement(equa_tr_t1954, equality_clause_scope__expansive_universalist, theater_ratio, 1954, 0.15).
narrative_ontology:measurement(equa_tr_t1968, equality_clause_scope__expansive_universalist, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(equa_tr_t2026, equality_clause_scope__expansive_universalist, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(equa_be_t1776, equality_clause_scope__expansive_universalist, base_extractiveness, 1776, 0.15).
narrative_ontology:measurement(equa_be_t1820, equality_clause_scope__expansive_universalist, base_extractiveness, 1820, 0.12).
narrative_ontology:measurement(equa_be_t1857, equality_clause_scope__expansive_universalist, base_extractiveness, 1857, 0.1).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__expansive_universalist, base_extractiveness, 1868, 0.45).
narrative_ontology:measurement(equa_be_t1896, equality_clause_scope__expansive_universalist, base_extractiveness, 1896, 0.3).
narrative_ontology:measurement(equa_be_t1954, equality_clause_scope__expansive_universalist, base_extractiveness, 1954, 0.55).
narrative_ontology:measurement(equa_be_t1968, equality_clause_scope__expansive_universalist, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(equa_be_t2026, equality_clause_scope__expansive_universalist, base_extractiveness, 2026, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1776, equality_clause_scope__expansive_universalist, suppression_requirement, 1776, 0.1).
narrative_ontology:measurement(equa_su_t1820, equality_clause_scope__expansive_universalist, suppression_requirement, 1820, 0.08).
narrative_ontology:measurement(equa_su_t1857, equality_clause_scope__expansive_universalist, suppression_requirement, 1857, 0.05).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__expansive_universalist, suppression_requirement, 1868, 0.55).
narrative_ontology:measurement(equa_su_t1896, equality_clause_scope__expansive_universalist, suppression_requirement, 1896, 0.25).
narrative_ontology:measurement(equa_su_t1954, equality_clause_scope__expansive_universalist, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1968, equality_clause_scope__expansive_universalist, suppression_requirement, 1968, 0.65).
narrative_ontology:measurement(equa_su_t2026, equality_clause_scope__expansive_universalist, suppression_requirement, 2026, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional equality' decomposes, per the epsilon-invariance principle, into at least three structurally distinct constraints sharing one kernel text: this expansive universalist reading (universal beneficiary set, judicial expansion at a low threshold, epsilon 0.30 assessed by its own lights); the restrictive originalist reading (qualified beneficiary set, historically bound scope, an entirely different victim set); and the progressive textualist reading (universal-in-principle beneficiary set, amendment-gated expansion, different payer set). The readings are linked as a constraint family: the originalist reading is the standing historical arrangement this reading contests and is cited by its opponents as the clause's true meaning, while the textualist reading is the rival expansion mechanism competing for the same reform constituency. Each reading's epsilon is stable because each is measured against its own referent, never averaged across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, institutional, 0.2).
constraint_indexing:directionality_override(equality_clause_scope__expansive_universalist, powerless, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
