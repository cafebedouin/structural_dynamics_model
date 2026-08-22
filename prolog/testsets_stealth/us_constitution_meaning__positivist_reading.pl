% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Positivist Reading of Constitutional Validity — Enactment-Procedural Criterion
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   us_constitution_meaning: the positivist reading, on which constitutional
 *   validity flows from formal enactment procedures and institutional
 *   authority rather than external moral principles. The standing arrangement
 *   under contest — the referent for epsilon — is that criterion as it
 *   actually governs: courts validate or reject official action by asking
 *   what was duly enacted, and decline validity to claims whose moral force
 *   lacks textual-procedural anchor. The reading delivers a real coordination
 *   good (a determinate, publicly accessible test of what counts as
 *   constitutional law) while systematically transferring interpretive
 *   authority away from moral reasoners and toward holders of enactment
 *   credentials; substantive justice claimants without textual support bear
 *   the cost. Per the epsilon-invariance principle this is one of three
 *   linked stories: the originalist reading (meaning fixed at ratification)
 *   and the living-constitutionalist reading (application evolves) are
 *   separate constraints with their own epsilon values and victim sets,
 *   linked via network.affects_constraints. Claim/metric independence is
 *   preserved: claimed_type records the structure believed true
 *   (tangled_rope); the metrics record the operation believed descriptively
 *   accurate. On the receipt surface, the gains demonstrably accrue to the
 *   federal_judiciary seat (legitimacy rents and docket-defining authority),
 *   and fixing — whether by amendment or by judicial abandonment of the
 *   criterion — is prohibitively costly relative to its benefit.
 *
 * KEY AGENTS:
 *   - federal_judiciary: agenda-setter and principal collector (institutional / identity_locked) — administers the validity test, collects legitimacy and docket-defining authority
 *   - elected_legislatures: secondary beneficiary (institutional / constrained) — holds the exclusive amendment credential shielding enactments
 *   - unenumerated_rights_claimants: primary target (powerless / trapped) — bears foreclosure of justice claims lacking textual anchor
 *   - amendment_seeking_reform_movements: secondary target (organized / trapped) — bears the frozen-corpus cost of a theoretically open but practically closed amendment channel
 *   - ordinary_citizens: diffuse beneficiary-payer (moderate / constrained) — consumes predictable law, absorbs textual silence
 *   - moral_jurisprudence_scholars: excluded voice (moderate / mobile) — argues for moral sources of validity from outside the conversation
 *   - jurisprudential_analysts: analytical observer (analytical / analytical) — maps the criterion's operation without deciding anything
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.62).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Positivist Reading of Constitutional Validity — Enactment-Procedural Criterion").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '65ea876c-fa8b-4a9c-a3b4-dbcf650668ff').
narrative_ontology:cs_kernel_codification('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', fixed_text).
narrative_ontology:cs_authority_grounding('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', practice).
narrative_ontology:cs_interpretation_layer_present('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff').
narrative_ontology:cs_reading_relation('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', us_constitution_meaning__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_axiom('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', foundational, validity_from_enactment_not_morality).
narrative_ontology:cs_axiom_status(validity_from_enactment_not_morality, holdable).
narrative_ontology:cs_axiom_grounding('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', validity_from_enactment_not_morality, conventional).
narrative_ontology:cs_axiom('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', secondary, article_v_exclusive_change_channel).
narrative_ontology:cs_axiom_status(article_v_exclusive_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', article_v_exclusive_change_channel, conventional).
narrative_ontology:cs_reference_frame('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', procedural_enactment_authority).
narrative_ontology:cs_drift_state('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', contemporary_article_v_gridlock, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('65ea876c-fa8b-4a9c-a3b4-dbcf650668ff', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, elected_legislatures).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, amendment_seeking_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, ordinary_citizens).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, ordinary_citizens).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, procedural_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, hart_rule_of_recognition).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides what counts as constitutional law in every case before it, and its authority rests on presenting its work as applying duly enacted text rather than personal moral conviction. Its prestige, its confirmation politics, and its self-understanding are bound to that posture; stepping outside it would spend legitimacy the institution cannot readily rebuild. It sets the working rules of validity through its own precedents.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Pass statutes that enjoy a presumption of validity under the enacted-text test, and together with the states hold the only formal channel for changing the Constitution's content. In a polarized era that channel rarely produces amendments, but the exclusive credential still shields their enactments from challenge on moral grounds and lets them claim democratic authorization for whatever survives.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, elected_legislatures, beneficiary,
    institutional, biographical, constrained, national).

% Bring claims whose moral urgency outruns any enumerated clause — subsistence, shelter, relational and environmental interests with no textual home. The courts are the only body that could give these claims constitutional force, and the governing test instructs rejection whenever no enactment supports them. There is no other forum where the claim could succeed.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Organize to change constitutional content through the one formal channel available. Supermajority requirements spanning two chambers, two-thirds of the states, and a polarized electorate make success rare; generations of organizing routinely end without textual change, while moral persuasion outside the channel yields no legal result at all.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, amendment_seeking_reform_movements, payer,
    organized, generational, trapped, national).

% Live under predictable, publicly stated limits on government and can check official action against criteria anyone can read. They also absorb the downside when their own unrepresented interests lose to textual silence, and they cannot leave the jurisdiction whose legal order defines their rights.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, ordinary_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, ordinary_citizens, payer).

% Argue in books, journals, and classrooms that validity should answer to moral principle, and supply the intellectual scaffolding for dissents that invoke it. They hold no seat in the official validity conversation; their influence stops at persuasion, and the governing test's whole point is that persuasion of this kind confers no legal status.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, moral_jurisprudence_scholars, excluded,
    moderate, biographical, mobile, global).

% Study the validity criterion from outside the dispute — comparing it with other legal systems' tests for what counts as law, tracking which arguments win over time, and mapping where the criterion's promises hold and fail. They decide nothing and collect nothing.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, jurisprudential_analysts, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, publicly ascertainable test for what counts as constitutional law: an act or claim is validated by tracing it to duly enacted text and prescribed procedure. Officials, courts, and citizens can predict validity without adjudicating moral merit, and disputes resolve by pointing to the enactment record.
% TRANSFER_FUNCTION: Moves interpretive and outcome-determinative authority from moral reasoners — claimants, scholars, citizens pressing unrepresented interests — to holders of enactment credentials: courts that apply enacted text and legislatures that control the amendment channel. It also moves legitimacy: judicial decisions borrow the democratic pedigree of enactment.
% ABSENT_VOICES: Unenumerated-rights claimants and moral jurisprudents (natural-law and Dworkinian theorists) would object that validity answers to justice, not pedigree; they stand outside the conversation — their arguments surface only as dissents and commentary, never as validity sources. Maintaining their exclusion is a substantial part of what the criterion's enforcement consists in.
% DISAPPEARANCE_RATIONALE: If the enactment-procedural criterion vanished overnight, adjudication would reorganize around some other validity source — moral readings, natural law, or overt living-constitutionalism; unenumerated claims would gain forums; the elected branches' monopoly on constitutional change would face competition; and the judiciary's neutral-applier legitimacy economy would collapse and need replacement.
% FOUNDING_PROBLEM: After legal realism exposed that appeals to moral principle in constitutional adjudication looked like judicial preference, the discipline needed an account of what constitutional law IS — determinate enough to constrain judges, and respectful enough of democracy to explain why unelected courts may invalidate enactments.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians across schools corroborate the mid-century indeterminacy crisis the reading was built to answer. Critical scholars outside the beneficiary set — Dworkinian, natural-law, and critical-legal-studies jurists — attest that the promised determinacy was never achieved and that the criterion functions as closure against unrepresented claims; their testimony is adversarial, which is what makes it corroboration rather than cover. No attesting source exists within the beneficiary set alone.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.62) is substantial but not maximal: the criterion forecloses an entire class of justice claims and transfers legitimacy to enactment-holders, yet it also underwrites predictability and democratic accountability that every seat consumes. Suppression (0.58) is structural rather than coercive — maintained through professional discipline, appointment incentives, and the legitimacy economy of the neutral-applier role, not force; alternatives survive in scholarship and dissents, which is why accessibility_collapse (0.42) stays below the natural-law band and resistance (0.60) stays high. Theater (0.31) reflects a mostly functional procedural machinery whose neutrality rhetoric grows increasingly performative as outcomes track appointment politics. The three temporal series share one seven-point grid (interval 0-60 approximates 1965-2025): extractiveness dips at t=30 as pluralist coalitions briefly admit moral reasoning, then climbs as Article V gridlock freezes the enacted corpus and the reading converges on originalist practice; the suppression requirement ratchets upward as excluding moral reasoning demands more active maintenance against living-constitutionalist pressure. Identity-lock dynamics bind the judiciary: its institutional self-concept is fused with the neutral-applier posture, so exiting the criterion would cost the legitimacy that constitutes its authority — the lock stabilizes its beneficiary-side position rather than amplifying extraction against it.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is the rule of law itself: the judiciary experiences the criterion as constitutive of its authority, not as a burden — computed extraction from that seat sits near the subsidy end. From the payer seats the identical structure operates as closure: a claimant with a compelling unrepresented interest meets a forum instructed to say the claim is not law. Same text, same courtrooms, opposite experienced types — the engine computes this divergence from power, exit, and role data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries derive low d: the judiciary (agenda_setter, collecting legitimacy and docket-defining authority) and elected legislatures (holding the exclusive amendment credential that shields enactments). Victims derive high d, amplified by trapped exit: unenumerated claimants have no alternative forum that could confer constitutional status, and reform movements face a formally exclusive channel that gridlock has made theoretical. Ordinary citizens sit near symmetric — public criteria and democratic pedigree on one side, exposure to textual silence on the other. No directionality overrides were used: the role and exit declarations already differentiate the two institutional seats (administrator-collector versus credential-holder), and an override keyed to their shared institutional power atom could not distinguish them.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification guards against mislabeling in both directions. Reading the arrangement as pure extraction would erase the genuine coordination function — determinate validity criteria are a real good that even the constraint's victims consume when they rely on knowing what the law is. Reading it as pure coordination would erase the systematic foreclosure: the same criterion that coordinates also disqualifies a whole class of claimants by design, and requires active enforcement to keep moral sources out. Tangled rope holds both facts. On the R5 interview the founding problem (post-realist indeterminacy and the legitimacy of judicial review) is contested rather than dead: parties dispute whether positivism ever delivered the determinacy it promised, so no dead-mandate mismatch arises against the world_rearranges verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the kernel us_constitution_meaning; what structural differences would instantiating the originalist or living-constitutionalist sibling produce?',
    'Author and compare the sibling stories: victim sets, epsilon, and persistence profiles differ by reading; cross-read the three files'' classifications.',
    'Under the originalist sibling, victims shift to those harmed by frozen historical meaning and epsilon tracks the ratification gap; under the living sibling, victims shift to those harmed by unpredictable evolution. The classification of this file applies only to the positivist instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-contingency of the classification within the us_constitution_meaning kernel.').

omega_variable(
    amendment_gridlock_collapse,
    'Does the positivist reading remain a distinct operating constraint, or has it collapsed into originalism in practice now that the Article V channel is effectively gridlocked?',
    'Compare positivist-grounded and originalist-grounded majority opinions in cases where enacted text is silent or outdated: sustained convergence indicates collapse.',
    'If collapsed, the effective constraint governing practice is the originalist sibling''s (different epsilon, different victim set), and this story''s metrics describe a form whose independent operation has ended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gridlock_collapse, empirical, 'Whether gridlock has fused the positivist reading with originalist practice.').

omega_variable(
    rule_of_recognition_depth,
    'Is the enactment-procedural criterion actually the shared rule of recognition among US officials, or one legitimating vocabulary invoked opportunistically alongside others?',
    'Systematic coding of validity arguments in majority opinions and official conduct: a stable criterion shows consistent invocation across ideological blocs; opportunism shows bloc-correlated switching between pedigrees.',
    'If opportunistic, measured extraction reflects selective invocation rather than a stable constraint, and the coordination-function half of the tangled-rope structure weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rule_of_recognition_depth, empirical, 'Depth of official acceptance of the positivist criterion as the rule of recognition.').

omega_variable(
    open_texture_indeterminacy,
    'How much of the Constitution''s operative content is genuinely fixed by enactment, and how much is open-textured enough to admit construction even on positivist terms?',
    'Doctrinal mapping of clauses whose application is settled by text alone versus those requiring judicial construction (due process, equal protection, executive power).',
    'High open texture means the criterion constrains less than claimed, lowering effective extraction; near-complete fixity means the foreclosure of unrepresented claims approaches total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_texture_indeterminacy, empirical, 'Share of constitutional content fixed by enactment versus open to construction.').

omega_variable(
    self_assessed_extraction_discount,
    'The positivist reading denies legal status to claims lacking textual anchor — does it therefore count their foreclosure as extraction at all, or price epsilon near zero from its own seat?',
    'Conceptual: settle whether epsilon is indexed to the reading''s self-assessment or to the structural account of the standing arrangement; empirically: compare this file''s epsilon with one authored from inside the positivist seat.',
    'If epsilon is read as self-assessed, the arrangement prices as nearly non-extractive and the tangled-rope reading rests entirely on the structural victim declarations; if structurally indexed, the authored 0.62 stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(self_assessed_extraction_discount, conceptual, 'Reading-indexed epsilon tension: the reading''s own lights versus the structural account of its victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.17).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__positivist_reading, theater_ratio, 60, 0.31).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.59).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__positivist_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.49).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.56).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__positivist_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'what makes the Constitution binding': the label conflates three structurally distinct claims with different epsilon values and victim sets. This story authors the positivist reading (validity from enactment procedures; victims are claimants lacking textual anchor). The originalist reading (meaning fixed at ratification; victims are those harmed by frozen historical meaning) and the living-constitutionalist reading (application evolves; victims are those harmed by unpredictable evolution) are separate files. Upstream/downstream structure: the positivist reading's proceduralism, under Article V gridlock, channels practice toward the originalist sibling, while coexisting with the living sibling as a live rival neither side can eliminate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
