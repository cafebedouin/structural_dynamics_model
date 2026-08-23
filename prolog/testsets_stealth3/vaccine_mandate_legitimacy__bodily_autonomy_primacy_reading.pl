% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy — Bodily Autonomy Primacy Reading (Categorical Medical Self-Sovereignty)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This story instantiates the bodily-autonomy-primacy reading of the
 *   vaccine-mandate-legitimacy kernel as a single clean constraint: medical
 *   self-sovereignty is absolute, and state coercion of medical intervention
 *   is categorically impermissible regardless of outcome. The constraint
 *   operates in the public-health governance space as a universal rights
 *   floor — a shield of bodily sovereignty that no collective-outcome
 *   calculation may pierce — maintained through constitutional litigation,
 *   doctrinal advocacy, and legislative entrenchment rather than through
 *   penal machinery applied to persons. Its operation has a dual structure
 *   the classification must hold together: it coordinates a genuine
 *   collective good (the consent requirement for medical intervention, and
 *   the anti-eugenic backstop that no majority may conscript bodies), and
 *   through that same structure it transfers the burden of epidemic
 *   protection onto those who cannot vaccinate — concentrating unchosen
 *   exposure risk on the immunocompromised and the medically exempt — while
 *   doctrinal victories, membership growth, and funding concentrate in the
 *   liberty-advocacy organizations that litigate the frame. Per the
 *   kernel-reading epsilon-referent rule, epsilon is authored over the fixed
 *   kernel referent (the standing vaccine-mandate arrangement) assessed by
 *   this reading's own lights; the constraint's own operative structure is
 *   carried by the beneficiary and victim declarations and the stakeholder
 *   surface below. This file is one member of a three-reading constraint
 *   family; see commentary.kernel_context and network.dual_formulation_note.
 *
 * KEY AGENTS:
 *   - liberty_advocacy_movements: primary beneficiary (organized/identity_locked) — collects doctrinal vindication, membership, and funding; sets the litigation agenda that maintains the categorical frame
 *   - immunocompromised_patients: primary target (powerless/trapped) — bear unchosen exposure risk when the categorical bar blocks protective mandates; vaccine-nonresponsive, unable to exit the risk environment
 *   - medically_exempt_individuals: secondary target (powerless/trapped) — contraindicated from vaccination, dependent on others' compelled uptake for protection
 *   - state_public_health_authorities: constrained institutional actor (institutional/constrained) — loses mandate authority where the frame holds; bears disease burden and operational costs
 *   - constitutional_courts: agenda-setter (institutional/analytical) — adjudicate whether the categorical autonomy frame or the Jacobson police-power lineage governs
 *   - general_public: dual-positioned (moderate/constrained) — receives the bodily-sovereignty shield and bears diffuse indirect exposure cost
 *   - unvaccinated_conscientious_objectors: beneficiary (moderate/constrained) — receives the exemption shield the categorical frame provides
 *   - public_health_epistemic_community: excluded voice (organized/mobile) — produces outcome and proportionality evidence the categorical frame refuses by construction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.88).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.58).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Legitimacy — Bodily Autonomy Primacy Reading (Categorical Medical Self-Sovereignty)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '7d4e89e9-d574-4c30-9c8c-6f10de083086').
narrative_ontology:cs_kernel_codification('7d4e89e9-d574-4c30-9c8c-6f10de083086', formalized).
narrative_ontology:cs_authority_grounding('7d4e89e9-d574-4c30-9c8c-6f10de083086', lineage).
narrative_ontology:cs_interpretation_layer_present('7d4e89e9-d574-4c30-9c8c-6f10de083086').
narrative_ontology:cs_reading_relation('7d4e89e9-d574-4c30-9c8c-6f10de083086', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('7d4e89e9-d574-4c30-9c8c-6f10de083086', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('7d4e89e9-d574-4c30-9c8c-6f10de083086', foundational, state_medical_coercion_categorically_impermissible).
narrative_ontology:cs_axiom_status(state_medical_coercion_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('7d4e89e9-d574-4c30-9c8c-6f10de083086', state_medical_coercion_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('7d4e89e9-d574-4c30-9c8c-6f10de083086', secondary, medical_intervention_requires_individual_consent).
narrative_ontology:cs_axiom_status(medical_intervention_requires_individual_consent, holdable).
narrative_ontology:cs_axiom_grounding('7d4e89e9-d574-4c30-9c8c-6f10de083086', medical_intervention_requires_individual_consent, deontological).
narrative_ontology:cs_reference_frame('7d4e89e9-d574-4c30-9c8c-6f10de083086', medical_self_sovereignty_baseline).
narrative_ontology:cs_drift_state('7d4e89e9-d574-4c30-9c8c-6f10de083086', contemporary_post_covid_mandate_litigation, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('7d4e89e9-d574-4c30-9c8c-6f10de083086', '2026-08-10T00:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_patients).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_exempt_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, general_public).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_conscientious_objectors).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_authorities).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, general_public).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, bodily_integrity_doctrine).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, informed_consent_principle).
narrative_ontology:constraint_vindicates(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, nuremberg_consent_lineage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Organizations and coalitions that litigate categorical autonomy claims, draft model legislation, and mobilize members against compelled medical intervention. Doctrinal victories convert directly into membership growth, small-donor funding, and institutional standing; the categorical frame is the movement's unifying banner and the litigation pipeline is the organizations' revenue stream. Leaving the frame would mean dissolving the identity and the funding base the organizations are built on.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, agenda_setter).

% Adjudicate challenges to mandate authority and decide whether the categorical autonomy frame or the Jacobson police-power lineage governs. Each holding entrenches or erodes one frame; the courts bear no direct cost or gain from either outcome but determine which arrangement operates.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Administer vaccination programs and hold statutory mandate authority (school entry, employment conditions, emergency powers). Where the categorical frame is adopted, that authority is barred and the protective toolset shrinks; the agencies then bear the disease burden and operational disruption that voluntary uptake fails to prevent.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, state_public_health_authorities, payer,
    institutional, generational, constrained, national).

% Cannot mount protective responses to vaccination or depend on others' uptake for indirect protection. When mandates are barred, their exposure risk rises with community transmission; they cannot exit the risk environment, cannot vaccinate into safety, and have no seat in a frame that classifies their harm as inadmissible outcome evidence.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_patients, payer,
    powerless, biographical, trapped, national).

% Hold contraindications that bar vaccination outright. Their protection depends entirely on compelled or voluntary uptake by everyone else; when the categorical frame removes compulsion, they absorb the difference as unchosen infection risk with no available exit.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, medically_exempt_individuals, payer,
    powerless, biographical, trapped, national).

% Holds the bodily-sovereignty shield the frame maintains — no one may conscript their bodies — and bears the diffuse indirect cost of reduced collective protection: higher transmission, disrupted services, and risk to vulnerable people they know. Their choice set is bounded by whatever doctrine the courts adopt.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, general_public, payer).

% Receive the exemption shield the categorical frame provides: no penalty, exclusion, or condition attaches to their refusal. Compliance remains available at the cost of the conviction the refusal expresses, which makes exit nominally open and practically identity-priced.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, unvaccinated_conscientious_objectors, beneficiary,
    moderate, biographical, constrained, national).

% Epidemiologists, bioethicists, and modeling groups who produce transmission, coverage, and proportionality evidence. The categorical frame refuses outcome-based argument by construction, so their evidence never enters the frame's adjudication; they publish and operate in adjacent professional and international venues instead.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_epistemic_community, excluded,
    organized, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a universal rights floor against state-compelled medical intervention: every individual holds bodily sovereignty that no collective-outcome calculation may override. Solves the collective-action problem of preventing states and majorities from conscripting bodies — a protection each individual cannot secure alone against the state's emergency and police powers.
% TRANSFER_FUNCTION: Transfers the protection burden: when the frame bars mandates, the work of epidemic protection shifts from state-compelled vaccination to voluntary uptake, and the residual risk concentrates on those who cannot vaccinate. Separately, doctrinal victories, membership, and funding move from the contested policy space into liberty-advocacy organizations.
% ABSENT_VOICES: The immunocompromised and medically exempt have no seat in the categorical frame's adjudication: the frame refuses outcome evidence by construction ('regardless of outcome'), so the people who bear the exposure costs the frame generates cannot state their harm in the frame's own terms — their harm is an outcome, and outcomes are inadmissible. The public-health epistemic community arguing proportionality is excluded on the same ground.
% DISAPPEARANCE_RATIONALE: If the categorical frame vanished overnight, mandate authority would revert to the Jacobson baseline with little litigation friction; liberty-advocacy organizations would lose the unifying frame that organizes their litigation and identity, fragmenting into narrower privacy and due-process projects; and immunocompromised and medically exempt individuals would recover the herd-immunity protection that blanket mandates provide.
% FOUNDING_PROBLEM: State-compelled medical intervention without consent. The categorical norm was built against the eugenics-era record — forced sterilization programs authorized under Buck v. Bell and the compulsory-vaccination regimes that treated bodily refusal as a police matter — with the common-law bodily-integrity rule (Union Pacific Ry. v. Botsford: no right more sacred than control of one's own person) as its doctrinal anchor. It was built to make bodily sovereignty non-negotiable precisely so that outcome-based arguments could never reopen the door the eugenics era walked through.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the pre-controversy common-law bodily-integrity lineage itself (Botsford, 1891), the documented eugenics-era record (Buck v. Bell and the sterilization programs it authorized, established by historians rather than by liberty movements), and the international informed-consent lineage in medical research (Nuremberg Code and successors). Public-health authorities corroborate that state coercion in medicine is a real historical phenomenon while disputing that routine vaccination mandates instance it — hence 'contested' rather than 'live'.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon 0.88 is reading-indexed over the shared kernel referent: by this reading's lights the state's extraction of bodily compliance under penalty admits no offsetting justification, and the arrangement's extraction deepened as mandate scope expanded from school entry to employment conditions and civil exclusion. Suppression 0.58 is authored as a raw structural property, unscaled by power or scope: the categorical frame forecloses the mandate option wholesale — it admits no balancing by construction — but it is enforced through adjudication and advocacy rather than penal coercion of persons, which keeps it below the levels typical of enforced extraction regimes. Theater 0.30 measures the gap between the absolutist rhetoric ('regardless of outcome') and the movement's negotiated operative positions, which have historically accommodated school-entry mandates and quarantine powers; the categorical form is partly an identity signal, and the gap widened as the frame became a movement banner. Accessibility collapse 0.42 is low-moderate because the alternatives — proportionality review, risk-stratified mandates, duty-based authority — remain fully live and actively litigated; the categorical frame contests them rather than having collapsed them. Resistance 0.75 is high: the Jacobson police-power lineage, the public-health establishment, and most operative doctrine actively resist the categorical frame. The claimed type, tangled_rope, is authored independently from the structural data: a genuine coordination function (the rights floor) plus asymmetric extraction through the same structure (risk transferred to the vaccine-nonresponsive, gains captured by the movement), actively enforced. Coalition note for the powerless victim seats: immunocompromised patients are dispersed, medically fragile, and structurally voiceless inside the frame, which refuses their harm-claims as inadmissible outcome evidence — coalition formation through patient-advocacy organizations is possible but the frame's evidentiary refusal raises the cost of entry. The measurement series run on one shared time grid (T=0..60, approximately mid-1960s to mid-2020s) with all three metrics authored at every point: the base_extractiveness rise steepens at T=50-60 (COVID-era employment mandates and exclusion regimes), the theater rise tracks the frame's conversion into a movement banner, and the suppression_requirement rise tracks the enforcement machinery — the constitutional-litigation apparatus — the norm required as resistance grew.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same structure. From the liberty-advocacy seat the arrangement is a rights shield it built and litigates — a principle it experiences as fixed as physics. From the immunocompromised and medically-exempt seats the same structure operates as enforced risk-imposition: a protection they depended on was removed by a frame that refuses to hear their harm stated in outcome terms. From the state-public-health seat it is tool-removal — a protective instrument confiscated by doctrine. From the constitutional-courts seat it is an adjudicable doctrinal claim competing with the Jacobian lineage. The engine computes this per-seat divergence from the structural data (power, exit, directionality); the divergence between the movement's self-description and the vulnerable seats' experience is the measurement this story exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   liberty_advocacy_movements sit at the beneficiary end (d near 0.0): the frame's operation subsidizes them — vindication, membership, funding — and their identity_locked exit means the benefit is identity-constituted rather than incidental. unvaccinated_conscientious_objectors and general_public hold diffuse beneficiary positions (the exemption shield and the bodily-sovereignty floor), with general_public dual-positioned as diffuse payer of indirect exposure cost. The victim seats — immunocompromised_patients and medically_exempt_individuals — sit at the target end (d near 1.0): they bear unchosen exposure risk with trapped exit (vaccine-nonresponsive or contraindicated; no exit from the disease environment). state_public_health_authorities are institutional payers (lost mandate authority, borne disease burden) at high d with constrained exit. The declarations map directly onto the expected structural delta for this reading: the immunocompromised enter the victim set, high exposure risk is borne by the vulnerable, and the liberty-advocacy movement is the beneficiary — the mirror image of the public-health sibling's structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents both mislabelings. Reading the constraint as pure coordination (the movement's self-description: a pure rights floor) would hide the asymmetric extraction — the risk transferred to the vaccine-nonresponsive and the concentrated advocacy gains — that rides the same structure. Reading it as pure extraction (the public-health counter-description: pure obstruction) would hide the genuine coordination function — the consent requirement and the anti-eugenic backstop are real collective goods with real historical warrant. On mandatrophy: the founding problem (state-compelled medical intervention without consent — the eugenics-era record) is genuinely disputed-live: liberty movements attest it is live wherever emergency powers and employment mandates reach; public-health authorities attest the live problem is the reverse (epidemic harm from under-protection). The status is authored 'contested' rather than 'dead', so no zombie flag is warranted: the constraint persists because the founding dispute persists, not because anyone is maintaining an empty shell. If the founding dispute were resolved — either by a settled proportionality consensus or by categorical entrenchment — the constraint would resolve into pure coordination (if the rights floor held with the extraction priced) or pure extraction (if the categorical form persisted purely as movement infrastructure).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint instantiates one reading (bodily_autonomy_primacy_reading) of the kernel vaccine_mandate_legitimacy: what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the constraint family: classify the sibling stories (vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading) and diff victim sets, epsilon values, and seat structures against this file.',
    'Under the public-health sibling the victim set inverts (mandate-refusers become the penalized externality-bearing set; the state apparatus becomes beneficiary); under the risk-stratification sibling the victim set becomes the threshold-misclassified. The disagreement is located at a single structural point — the admissibility of outcome-based justification — and this reading''s categorical refusal of that admissibility is what generates its distinct victim set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the vaccine-mandate-legitimacy kernel, with sibling readings as separate constraints.').

omega_variable(
    categorical_vs_operative_autonomy_norm,
    'Is the operative constraint the categorical norm (state coercion impermissible regardless of outcome) or a softer autonomy-protective norm that the absolutist rhetoric overlays?',
    'Code operative positions — litigated holdings, legislative proposals, movement demands — for categorical versus balancing structure; compare the frame the courts actually adopt against the frame the rhetoric asserts.',
    'If the operative norm admits balancing, epsilon drops toward the risk-stratification range, the victim set thins, and the constraint moves toward pure coordination; the categorical form is precisely what generates the immunocompromised victim set and the foreclosure of both siblings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_operative_autonomy_norm, conceptual, 'Whether the categorical form or a negotiated balancing form is the constraint actually in operation.').

omega_variable(
    exposure_risk_attribution,
    'How much of the immunocompromised population''s exposure risk is attributable to the categorical norm''s operation (blocked mandates) versus baseline disease ecology and voluntary-uptake shortfalls?',
    'Counterfactual epidemiological modeling stratified by vaccine-failure status across mandate and no-mandate regimes, holding circulation variants and coverage baselines fixed.',
    'If most risk is background, the victim declaration overstates the constraint''s extraction and the structure moves toward pure coordination; if mandate-blocking is a major driver, the asymmetric-extraction reading holds and the tangled structure is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exposure_risk_attribution, empirical, 'Attribution of the victim set''s exposure risk between the constraint''s operation and background disease ecology.').

omega_variable(
    liberty_movement_identity_lock_depth,
    'Is the liberty movement''s identity lock ideological (the categorical frame constitutes members'' worldview) or institutional (the organizations are constituted by the litigation-funding stream)?',
    'Organizational funding-shock analysis: track whether movement positions soften toward proportionality when litigation revenue diversifies or collapses, versus holding across funding regimes.',
    'An institutional lock is breakable by funding shifts — the frame would soften toward risk-stratified proportionality and the victim set would thin; an ideological lock persists across funding regimes and keeps the constraint categorical and the foreclosure of siblings intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberty_movement_identity_lock_depth, empirical, 'Depth and mechanism of the beneficiary seat''s identity lock on the categorical frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(vacc_tr_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(vacc_tr_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(vacc_tr_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(vacc_tr_t40, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 40, 0.25).
narrative_ontology:measurement(vacc_tr_t50, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(vacc_tr_t60, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 60, 0.3).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(vacc_be_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 20, 0.72).
narrative_ontology:measurement(vacc_be_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 30, 0.74).
narrative_ontology:measurement(vacc_be_t40, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 40, 0.76).
narrative_ontology:measurement(vacc_be_t50, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 50, 0.8).
narrative_ontology:measurement(vacc_be_t60, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 60, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(vacc_su_t20, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 20, 0.42).
narrative_ontology:measurement(vacc_su_t30, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 30, 0.46).
narrative_ontology:measurement(vacc_su_t40, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement(vacc_su_t50, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 50, 0.54).
narrative_ontology:measurement(vacc_su_t60, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'vaccine mandate legitimacy' decomposes into three structurally distinct constraints per the epsilon-invariance principle: the categorical autonomy prohibition (this story), the duty-based mandate authority (public_health_primacy_reading), and the risk-threshold-contingent authority (risk_stratification_reading). Each reading authors its own epsilon over the shared kernel referent — the standing mandate arrangement — with this reading assessing it as categorically extractive (0.88), the public-health reading as justified coordination, and the risk-stratification reading as conditionally legitimate. Victim sets differ structurally across the family: this reading's constraint places the immunocompromised and medically exempt in the victim set (they bear the exposure risk the categorical bar generates); the public-health reading places the mandate-refusing in the penalized set. Each file is a clean, single-epsilon constraint; the stories are linked here as one family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
