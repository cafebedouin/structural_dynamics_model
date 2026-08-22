% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Right as Minoritarian Privilege Veto (Crown Reading)
 *   domain: constitutional history/political economy/legal authority
 *
 * SUMMARY:
 *   In eighteenth-century France, royal edicts — above all fiscal ones —
 *   required registration by the sovereign courts before taking effect, and
 *   the courts claimed the right to remonstrate, suspending registration
 *   pending royal response. This file instantiates the CROWN READING of the
 *   remonstrance_authority kernel: the remonstrance right as practiced is an
 *   illegitimate minoritarian veto wielded by a few hundred venal, hereditary
 *   magistrates to protect particularist fiscal privileges — their own tax
 *   exemptions and those of the orders they belonged to or sheltered — at the
 *   expense of royal fiscal authority and unprivileged taxpayers. The epsilon
 *   referent is the standing remonstrance arrangement as the crown reading
 *   assesses it, never the guardian-of-liberties arrangement the sibling
 *   reading would put in its place; that sibling is a separate constraint
 *   with its own file. The expected structural delta is honored: high epsilon
 *   against royal fiscal authority, the crown seated among the victims when
 *   thwarted, and magistrate legitimacy treated as the obstruction story
 *   rather than the ground truth.
 *
 * KEY AGENTS:
 *   - venal_parlement_magistrates: agenda-setter and principal beneficiary (institutional/identity_locked) — administers registration, decides remonstrance by chamber vote, collects office rents and procedural leverage
 *   - privileged_tax_exempt_orders: secondary beneficiary (powerful/constrained) — receives sheltered exemptions without operating the mechanism
 *   - crown_fiscal_authority: primary payer (institutional/constrained) — bears blocked taxation and fiscal shortfalls; counter-instruments exist but each burns legitimacy
 *   - commoner_taxpayers: payer (powerless/trapped) — absorbs the shifted burden with no procedural voice in registration
 *   - enlightenment_public_sphere: analytical observer — adjudicates the contest in print and supplies the constitutional idioms both camps deploy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.82).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.72).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Right as Minoritarian Privilege Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional history/political economy/legal authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'b46e79a2-0d06-49e0-9142-0a4aa32d1aed').
narrative_ontology:cs_kernel_codification('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', formalized).
narrative_ontology:cs_authority_grounding('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', lineage).
narrative_ontology:cs_interpretation_layer_present('b46e79a2-0d06-49e0-9142-0a4aa32d1aed').
narrative_ontology:cs_reading_relation('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', foundational, registration_is_royal_grace_not_right).
narrative_ontology:cs_axiom_status(registration_is_royal_grace_not_right, holdable).
narrative_ontology:cs_axiom_grounding('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', registration_is_royal_grace_not_right, conventional).
narrative_ontology:cs_axiom('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', foundational, undivided_sovereignty_precludes_corporate_veto).
narrative_ontology:cs_axiom_status(undivided_sovereignty_precludes_corporate_veto, holdable).
narrative_ontology:cs_axiom_grounding('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', undivided_sovereignty_precludes_corporate_veto, deontological).
narrative_ontology:cs_reference_frame('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', undivided_royal_sovereignty).
narrative_ontology:cs_drift_state('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', pre_maupeou_fiscal_crisis, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b46e79a2-0d06-49e0-9142-0a4aa32d1aed', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_parlement_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, privileged_tax_exempt_orders).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, commoner_taxpayers).
narrative_ontology:constraint_vindicates(remonstrance_authority__crown_reading, corporate_privilege_immunity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary owners of judicial offices in the sovereign courts. They verify and register royal edicts, decide by chamber vote when to remonstrate, and discipline members who break ranks. Their offices are purchasable and bequeathable property whose market value rises with the court's powers, and their corporate identity as guardians of the realm's laws is bound up with family lineage and honor. Exiting means selling an office whose price depends on the very powers in question, or breaking with the corporation that secures their standing.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_parlement_magistrates, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, venal_parlement_magistrates, beneficiary).

% Clergy and sword nobility whose exemptions from the main direct taxes and reduced assessments on newer levies survive each fiscal crisis. They operate nothing in the registration process; the shelter reaches them through the courts' objections. Leaving the social order that confers exemption is not a live option, so their stake is passive but total.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, privileged_tax_exempt_orders, beneficiary,
    powerful, generational, constrained, national).

% The king and his councils must fund wars, debt service, and administration. Every proposal to broaden taxation meets suspension of registration, forcing a choice among forced-registration ceremonies, exiling courts, creating parallel tribunals, or retreating. Each instrument works but spends legitimacy and provokes alliances among the courts; the fiscal-military commitments cannot be abandoned, so the crown cannot walk away from the contest.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_authority, payer,
    institutional, generational, constrained, national).

% Households subject to the main direct tax and the bulk of indirect levies. They have no seat in the registration process and no elective tie to either the court or the council; both sides invoke their interest in argument. Escape runs through emigration, evasion, or purchase of exempt status — each costly and partial.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, commoner_taxpayers, payer,
    powerless, biographical, trapped, national).

% Pamphleteers, barristers, and men of letters who adjudicate the dispute in print. They take no part in registration, supply the constitutional idioms both camps deploy, and observe the whole structure from outside the chambers and the council.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, enlightenment_public_sphere, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, venal_parlement_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sequences royal will into enforceable law through a mandatory checkpoint: edicts are verified against enrolled law, publicly registered, and archived; the remonstrance phase inserts a suspension window in which the sovereign courts articulate objections before registration completes.
% TRANSFER_FUNCTION: Moves fiscal discretion and revenue security from the crown to protected private holders: each sustained remonstrance preserves tax exemptions and officeholder prerogatives, shifting wartime finance onto unprivileged taxpayers and royal borrowing; it also moves de facto agenda-setting power over taxation from the royal council to a few hundred hereditary judges.
% ABSENT_VOICES: Unprivileged taxpayers and the nation both sides invoke: they hold no procedural standing in registration and no elective connection to either the crown's council or the corporate court. Reform-minded royal ministers likewise enter the record only through the crown's voice. They are absent from the chambers where the veto is exercised and from the council where it is overridden.
% DISAPPEARANCE_RATIONALE: If the remonstrance right vanished overnight, royal fiscal authority would rearrange immediately: universal wartime taxes could reach the privileged orders, the courts' leverage over finance would collapse, office values would fall, and the distribution of the fiscal burden between orders would shift — the entire bargaining structure of French absolutism reorganizes around it.
% FOUNDING_PROBLEM: Originally: ensure royal edicts conformed to the realm's established law and were duly enrolled and publicized before binding subjects — a verification and publicity safeguard from an era of thin administrative capacity.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties: royal fiscal memoranda and controller-general correspondence treat the late-century remonstrances as pure obstruction carrying no verification content; the revealed preference of the revolutionary assemblies (abolition of venal offices and the parlements, 1790) and subsequent legal-historical scholarship corroborate that the verification function had decayed while the veto persisted. The magistrates' own remonstrances, by contrast, attest the founding problem as live — no source inside the beneficiary set corroborates 'dead', and that asymmetry is itself the signal this reading predicts.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82 at interval end) because sustained remonstrances systematically converted royal fiscal proposals into negotiated retreats that preserved order-specific exemptions, pushing wartime finance onto unprivileged shoulders and royal credit. Suppression (0.72) is structural: the suspension power suppresses royal fiscal initiative, and corporate discipline suppresses dissenting members within the courts. Theater (0.34) is real but sub-dominant — the guardianship idiom grew increasingly ceremonial relative to interest defense, yet the mechanism continued to do genuine blocking work, so this is not an inertially maintained shell. Accessibility collapse is moderate (0.45): the crown's alternatives (forced-registration ceremonies, exiling courts, creating parallel tribunals) remained visible and usable, but each carried heavy legitimacy cost and provoked cross-court coalitions, so alternatives degraded without collapsing. Resistance is high (0.68): constant royal counter-pressure across the interval, culminating in the 1771 abolition of the courts. The temporal series run on one shared nine-point grid. The dynamics are cyclical with a rising trend: fiscal crisis, tax proposal, remonstrance, royal counter-move, negotiated registration, calm, next crisis. The oscillation is itself part of the extraction mechanism — each resolved crisis re-entrenches the veto and raises the price of the next confrontation, an intermittent-reinforcement ratchet rather than noise. The suppression_requirement series traces the escalating enforcement contest deliberately: by interval end the coercive effort needed to hold the arrangement exceeded what ordinary enforcement could deliver, which is why the terminal move was abolition rather than enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently. From the magistrates' seat the arrangement is the constitution itself — the sibling file, authored from that seat, will classify the same practices with inverted beneficiary/victim sets and far lower epsilon. From the crown's seat and the taxpayer's seat the same structure operates as enforced extraction. Note also the same-level asymmetry: crown and parlement both hold institutional power, yet their exits differ sharply — the crown's instruments all spend legitimacy, while the magistrates' exit is identity-fused, since the office is simultaneously property, honor, and family lineage, making departure unthinkable without self-impoverishment and dishonor.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations drive the derivation: venal_parlement_magistrates (agenda-setter and beneficiary, identity_locked) sit near the beneficiary end; privileged_tax_exempt_orders (beneficiary, constrained) sit close beside them, receiving shelter without operating anything. The victim declarations drive the opposite pole: commoner_taxpayers (powerless, trapped) sit nearest the full-target end, bearing the shifted burden with no exit; crown_fiscal_authority (institutional, constrained) sits slightly below full-target because its counter-instruments partially hedge the extraction even though each use is costly. No directionality overrides are needed — the beneficiary/victim declarations plus exit options reproduce these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandate question by separating the founding function from the persisted function. The arrangement was built to verify and publicize edicts in an era of thin administrative capacity; by the interval's end that function was routine and vestigial, while the veto operated as privilege defense. Founding_problem_status dead combined with disappearance_verdict world_rearranges produces the capture/zombie signal: the arrangement persists because seats depend on it, not because its problem remains. This prevents mislabeling in both directions — it stops the crown reading from flattening the arrangement into mere obstruction-talk with no mechanism, and it stops a magistrate-seat story from reading the same practices as a live constitutional check. The corpus needs both files precisely so that divergence is measured rather than asserted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the crown_reading of the remonstrance_authority kernel; the magistrate_reading instantiates the same practices as a fundamentally different constraint. How much of the classification is reading-indexed rather than structure-indexed?',
    'Side-by-side comparison of the sibling files: identical practices, inverted beneficiary/victim sets, divergent epsilon. Cross-reading deltas locate which structural facts are shared and which are artifacts of the reading.',
    'Under the magistrate reading the crown leaves the victim set and enters as aggressor, epsilon falls toward coordination-cost levels, and the type claim shifts toward rope or tangled_rope; this story''s snare verdict is conditional on its own legitimacy premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Classification dependence on kernel-reading choice.').

omega_variable(
    veto_minoritarianism_basis,
    'Is the veto minoritarian as a structural property of the mechanism (a few hundred officeholders against crown and nation), or only in its crisis-period exercise — would the same mechanism look defensive of broad opinion under different use?',
    'Compare remonstrance practice across issue domains and periods: domains where objections tracked wide opinion versus domains where they tracked narrow fiscal interest.',
    'If minoritarianism is use-contingent, the pure-extraction claim weakens toward a hybrid coordination/extraction account; if structural, the snare characterization holds across periods.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_minoritarianism_basis, conceptual, 'Whether the minoritarian character is intrinsic to the mechanism or contingent on its exercise.').

omega_variable(
    representation_deficit_symmetry,
    'Both seats claim to act for the nation — the crown by dynastic-divine trusteeship, the magistrates by corporate guardianship. Does the crown reading''s ''illegitimate minoritarian veto'' framing presuppose a royal representative claim that is itself independently uncorroborated?',
    'Analysis of contemporaneous legitimacy argumentation (pamphlet wars, cahiers, doctrinal tracts) assessing which representative claim carried assent outside the claiming party.',
    'If the crown''s representative claim is no stronger than the magistrates'', the arrangement reads as a bilateral elite contest that jointly extracts from commoners, altering the victim structure so that both elite seats face the taxpayers rather than each other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(representation_deficit_symmetry, conceptual, 'Symmetry of the unrepresentative-character charge between crown and magistracy.').

omega_variable(
    interest_principle_composition,
    'What fraction of remonstrance activity was defense of corporate and order-specific interest versus sincere constitutional conviction?',
    'Content analysis of the remonstrance corpora across domains and decades, examining differential behavior where interest and principle aligned versus conflicted.',
    'A predominantly principled composition supports the sibling reading''s coordination account and lowers epsilon; a predominantly interested composition confirms the cover-story structure this reading asserts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interest_principle_composition, empirical, 'Interest-versus-principle composition of the remonstrance practice.').

omega_variable(
    counterfactual_fiscal_solvency,
    'Would removing the remonstrance veto actually have restored royal solvency, or were the insolvency drivers (war expenditure, debt service, tax-farming inefficiency) independent of the veto?',
    'Fiscal-counterfactual analysis of revenues under the abolished courts (1771-1774) and of jurisdictions without a suspension right (pays d''etat, newly created tribunals).',
    'If removal would not have improved solvency, the extraction-from-royal-fiscal-authority framing weakens — the veto becomes a co-symptom rather than a cause, lowering harm attribution to this arrangement specifically.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_fiscal_solvency, empirical, 'Whether the veto was causally load-bearing for royal fiscal distress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(remo_tr_t7, remonstrance_authority__crown_reading, theater_ratio, 7, 0.15).
narrative_ontology:measurement(remo_tr_t14, remonstrance_authority__crown_reading, theater_ratio, 14, 0.18).
narrative_ontology:measurement(remo_tr_t21, remonstrance_authority__crown_reading, theater_ratio, 21, 0.2).
narrative_ontology:measurement(remo_tr_t28, remonstrance_authority__crown_reading, theater_ratio, 28, 0.23).
narrative_ontology:measurement(remo_tr_t35, remonstrance_authority__crown_reading, theater_ratio, 35, 0.27).
narrative_ontology:measurement(remo_tr_t42, remonstrance_authority__crown_reading, theater_ratio, 42, 0.29).
narrative_ontology:measurement(remo_tr_t49, remonstrance_authority__crown_reading, theater_ratio, 49, 0.32).
narrative_ontology:measurement(remo_tr_t56, remonstrance_authority__crown_reading, theater_ratio, 56, 0.34).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(remo_be_t7, remonstrance_authority__crown_reading, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(remo_be_t14, remonstrance_authority__crown_reading, base_extractiveness, 14, 0.57).
narrative_ontology:measurement(remo_be_t21, remonstrance_authority__crown_reading, base_extractiveness, 21, 0.66).
narrative_ontology:measurement(remo_be_t28, remonstrance_authority__crown_reading, base_extractiveness, 28, 0.62).
narrative_ontology:measurement(remo_be_t35, remonstrance_authority__crown_reading, base_extractiveness, 35, 0.72).
narrative_ontology:measurement(remo_be_t42, remonstrance_authority__crown_reading, base_extractiveness, 42, 0.69).
narrative_ontology:measurement(remo_be_t49, remonstrance_authority__crown_reading, base_extractiveness, 49, 0.78).
narrative_ontology:measurement(remo_be_t56, remonstrance_authority__crown_reading, base_extractiveness, 56, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(remo_su_t7, remonstrance_authority__crown_reading, suppression_requirement, 7, 0.46).
narrative_ontology:measurement(remo_su_t14, remonstrance_authority__crown_reading, suppression_requirement, 14, 0.5).
narrative_ontology:measurement(remo_su_t21, remonstrance_authority__crown_reading, suppression_requirement, 21, 0.55).
narrative_ontology:measurement(remo_su_t28, remonstrance_authority__crown_reading, suppression_requirement, 28, 0.6).
narrative_ontology:measurement(remo_su_t35, remonstrance_authority__crown_reading, suppression_requirement, 35, 0.67).
narrative_ontology:measurement(remo_su_t42, remonstrance_authority__crown_reading, suppression_requirement, 42, 0.71).
narrative_ontology:measurement(remo_su_t49, remonstrance_authority__crown_reading, suppression_requirement, 49, 0.79).
narrative_ontology:measurement(remo_su_t56, remonstrance_authority__crown_reading, suppression_requirement, 56, 0.86).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the remonstrance_authority kernel decomposes into two readings with materially different epsilon. This file (crown_reading) assesses the standing remonstrance arrangement as an illegitimate minoritarian privilege veto — high epsilon, crown and taxpayers in the victim set. The sibling file (magistrate_reading) assesses the same practices as a fundamental constitutional mechanism — low-to-moderate epsilon, the crown appearing as potential aggressor rather than victim. Neither reading is evidentially upstream of the other; they are rival normative instantiations of one kernel, linked so that cross-reading comparison and contamination propagation operate. The crown reading's foundational premises logically exclude the magistrate reading's within any single constitutional framework, which is why the historical contest was settled by force rather than synthesis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
