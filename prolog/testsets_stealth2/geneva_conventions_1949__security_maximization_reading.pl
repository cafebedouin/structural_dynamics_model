% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__security_maximization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__security_maximization_reading, []).

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
 *   constraint_id: geneva_conventions_1949__security_maximization_reading
 *   human_readable: Security-Maximization Reading of the Geneva Conventions: Necessity-Override Regime
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Geneva Conventions 1949
 *   kernel: the security-maximization reading, under which treaty protections
 *   are peacetime aspirations that yield to operational necessity in
 *   asymmetric conflict. The constraint modeled here is the standing
 *   arrangement that reading produces — the necessity-override regime with
 *   its expanding unlawful-combatant category, degraded civilian immunity,
 *   indefinite detention, and definitional narrowing of prohibited
 *   interrogation. Per the epsilon-invariance principle, the sibling readings
 *   (humanitarian_ceiling_reading, conditional_reciprocity_reading) are
 *   separate constraints in separate files; nothing about them is averaged
 *   into this story's epsilon, whose referent is the necessity-override
 *   arrangement itself, assessed as it actually operates. Time units are
 *   years since 2001 (interval 0-25 maps to 2001-2026). KEY AGENTS (by
 *   structural relationship): - national_security_executive: Agenda-setter
 *   and primary beneficiary (institutional/arbitrage) — authors the necessity
 *   doctrine, collects the operational freedom -
 *   military_intelligence_operators: Secondary beneficiary
 *   (organized/constrained) — executes under legal cover, careers bound to
 *   the framework - coalition_partner_governments: Dual-positioned
 *   beneficiary/payer (powerful/mobile) — imports the cover, absorbs
 *   reciprocity blowback - detained_security_internees: Primary target
 *   (powerless/trapped) — bears status denial, indefinite detention, coercive
 *   interrogation - civilians_in_asymmetric_theaters: Primary target
 *   (powerless/trapped) — bears widened collateral exposure and shifted risk
 *   - domestic_judicial_review_bodies: Excluded voice
 *   (institutional/constrained) — jurisdiction narrowed away from the
 *   framework - icrc_humanitarian_monitors: Excluded voice
 *   (organized/constrained) — verification access refused or conditioned -
 *   human_rights_documentation_organizations: Analytical observer
 *   (organized/analytical) — documents from outside, compels nothing
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, 0.82).
domain_priors:suppression_score(geneva_conventions_1949__security_maximization_reading, 0.72).
domain_priors:theater_ratio(geneva_conventions_1949__security_maximization_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__security_maximization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__security_maximization_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__security_maximization_reading, "Security-Maximization Reading of the Geneva Conventions: Necessity-Override Regime").
narrative_ontology:topic_domain(geneva_conventions_1949__security_maximization_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__security_maximization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__security_maximization_reading, 'ec818e64-c81c-48e7-b351-947e29d2b24e').
narrative_ontology:cs_kernel_codification('ec818e64-c81c-48e7-b351-947e29d2b24e', fixed_text).
narrative_ontology:cs_authority_grounding('ec818e64-c81c-48e7-b351-947e29d2b24e', extraction).
narrative_ontology:cs_interpretation_layer_present('ec818e64-c81c-48e7-b351-947e29d2b24e').
narrative_ontology:cs_reading_relation('ec818e64-c81c-48e7-b351-947e29d2b24e', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('ec818e64-c81c-48e7-b351-947e29d2b24e', geneva_conventions_1949__conditional_reciprocity_reading, coexists_with).
narrative_ontology:cs_axiom('ec818e64-c81c-48e7-b351-947e29d2b24e', foundational, protections_yield_to_operational_necessity).
narrative_ontology:cs_axiom_status(protections_yield_to_operational_necessity, holdable).
narrative_ontology:cs_axiom_grounding('ec818e64-c81c-48e7-b351-947e29d2b24e', protections_yield_to_operational_necessity, instrumental).
narrative_ontology:cs_axiom('ec818e64-c81c-48e7-b351-947e29d2b24e', secondary, irregular_warfare_excludes_treaty_protections).
narrative_ontology:cs_axiom_status(irregular_warfare_excludes_treaty_protections, holdable).
narrative_ontology:cs_axiom_grounding('ec818e64-c81c-48e7-b351-947e29d2b24e', irregular_warfare_excludes_treaty_protections, conventional).
narrative_ontology:cs_reference_frame('ec818e64-c81c-48e7-b351-947e29d2b24e', sovereign_necessity_prerogative).
narrative_ontology:cs_drift_state('ec818e64-c81c-48e7-b351-947e29d2b24e', contemporary_asymmetric_conflict_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('ec818e64-c81c-48e7-b351-947e29d2b24e', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, national_security_executive).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, military_intelligence_operators).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__security_maximization_reading, coalition_partner_governments).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, detained_security_internees).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, civilians_in_asymmetric_theaters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(geneva_conventions_1949__security_maximization_reading, coalition_partner_governments).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, operational_necessity_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, unlawful_combatant_category).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__security_maximization_reading, human_shields_liability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the legal memoranda that define who counts as a combatant, what counts as detention, and what counts as interrogation. Directs military and intelligence operations under those definitions. Can reinterpret or narrow the framework at will, decline optional protocols, and shield past decisions behind state-secrets privilege. Gains operational freedom unbound by reciprocal restraint; bears periodic litigation and diplomatic friction.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, national_security_executive, agenda_setter,
    institutional, generational, arbitrage, global).

% Carry out detentions, interrogations, and strikes under the executive's definitions. Receive clear authorization and indemnification for actions taken within the defined categories. Career advancement runs through the chain of command that maintains the framework; refusing an authorized operation ends careers. Cannot individually alter the definitions they operate under.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, military_intelligence_operators, beneficiary,
    organized, biographical, constrained, global).

% Adopt the same necessity framing for their own deployments and intelligence-sharing arrangements, gaining cover for operations that would otherwise face domestic and treaty objections. At the same time their captured personnel become vulnerable to the same status-denial logic when adversaries invoke it back, and their publics carry reputational costs.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, coalition_partner_governments, beneficiary,
    powerful, biographical, mobile, continental).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__security_maximization_reading, coalition_partner_governments, payer).

% Held outside prisoner-of-war status under the unlawful-combatant designation, often at facilities beyond domestic jurisdiction. No habeas petition, no charge, no trial date; release depends on executive discretion. Subject to interrogation practices defined downward by the same memoranda that deny their status. Families and home states frequently cannot locate or reach them.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, detained_security_internees, payer,
    powerless, biographical, trapped, regional).

% Live where irregular fighters embed. When adversaries are framed as using human shields, casualty projections widen and strike thresholds loosen; collateral-damage estimates are calculated and accepted internally before residents know a strike occurred. Leaving means abandoning homes and livelihoods; staying means living inside the targeting picture.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, civilians_in_asymmetric_theaters, payer,
    powerless, generational, trapped, regional).

% Courts and commissions that would otherwise test detention and interrogation against constitutional and statutory limits. Jurisdiction narrowed through standing doctrines, venue selection, and classification rules; several review channels were created and then structured to defer to executive determinations. They can slow individual cases but cannot reach the framework itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, domestic_judicial_review_bodies, excluded,
    institutional, generational, constrained, national).

% Treaty-mandated visitors to places of detention. Access to the relevant facilities was refused, delayed, or conditioned in ways that broke confidentiality guarantees; registration of internees was incomplete. Their reporting channels exist precisely to verify treatment, and much of the arrangement's operation sits where their access does not reach.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, icrc_humanitarian_monitors, excluded,
    organized, generational, constrained, global).

% Compile detainee testimony, strike casualty records, and leaked-document analysis from outside the system. Publish findings, litigate where standing allows, and brief treaty bodies. No power to compel access or testimony; their account competes with official narratives in domestic and international forums.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__security_maximization_reading, human_rights_documentation_organizations, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__security_maximization_reading, national_security_executive).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__security_maximization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the authorization of state violence in asymmetric conflict: resolves inter-agency legality disputes through the necessity doctrine, gives operators uniform rules and legal cover, maintains alliance interoperability under a nominally shared treaty vocabulary, and preserves residual (degraded) protections that keep the framework diplomatically usable.
% TRANSFER_FUNCTION: Moves physical liberty (indefinite detention), bodily security (coercive interrogation), and life (widened collateral exposure) from detainees and civilian populations in conflict theaters to the state security apparatus as expanded operational freedom; moves legal risk from operators to the people the operations touch.
% ABSENT_VOICES: Detainees themselves have no standing anywhere in the process that defines their status; domestic courts are kept out through jurisdiction-narrowing; ICRC monitors are kept out through access denial; jurists holding the humanitarian-ceiling position are excluded from the executive legal process that produces the operative definitions.
% DISAPPEARANCE_RATIONALE: If the necessity-override practice vanished overnight, thousands of internees would have to be charged or released, interrogation and detention siting would change immediately, targeting collateral standards would tighten, coalition legal-cover arrangements would require renegotiation, and the treaty system's dormant verification machinery would reactivate.
% FOUNDING_PROBLEM: The kernel's founding problem was mutual restraint among belligerents protecting wounded, shipwrecked, captive, and civilian persons after the atrocities of two world wars. This reading's specific founding problem was narrower: the friction between an interstate-war treaty framework and counterterrorism against non-state actors, where executives found POW-status rules, detention limits, and interrogation boundaries obstructive to operations.
% FOUNDING_PROBLEM_CORROBORATION: From outside the benefiting parties: serving and former judge advocates general, ICRC reporting (including leaked portions), UN commission-of-inquiry findings, and mainstream IHL scholarship attest that the 1949 Conventions and the 1977 Protocols already supplied tools for irregular conflict (Common Article 3, status-determination procedures) and that the claimed gap was a policy choice. The still-live version of the founding problem is attested almost exclusively by the security agencies that benefit from the arrangement.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__security_maximization_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__security_maximization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__security_maximization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_1949__security_maximization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__security_maximization_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__security_maximization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__security_maximization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__security_maximization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.82) because the arrangement transfers liberty, bodily security, and life from people with no procedural recourse to an apparatus that faces none of those costs; the rate is decoupled from any verified security return. Suppression (0.72) is structural: jurisdiction-stripping, venue manipulation, access denial, and classification rules, not persuasion. Theater (0.45) is substantial and rising — formal compliance reviews that defer to executive determinations, investigation processes that rarely reach operational decisions, and official repudiation of specific techniques coexisting with unchanged detention and targeting practice — but real functional activity remains (operations are genuinely planned, authorized, and executed under the framework). Accessibility_collapse (0.6) is moderate: full compliance remained available to the state at all times (many parties to the same conflicts took it), so alternatives did not vanish; they were institutionally priced out. Resistance (0.55) reflects sustained habeas litigation, JAG and agency-internal dissent, ICRC advocacy, and treaty-body pressure that won real but partial concessions. The measurement series run on one shared grid (t=0,5,10,15,20,25): extractiveness ratchets upward through the black-site and status-denial period and plateaus high as practices entrench; theater grows steadily as performative compliance substitutes for substantive constraint; suppression_requirement rises while courts and civil society actively contested the program, then eases slightly as normalization reduced the need for active defense — enforcement decay through success, not through reform.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same text. From the agenda-setter seat the arrangement is self-authored flexibility: the constraint binds others, not its author, so it registers as barely a constraint at all. From the operator seat it is protective clarity — defined categories, indemnification, career continuity. From the payer seats it is unreviewable violence: no forum, no status, no exit. From the excluded reviewer seats it is a closed door. The engine computes these divergent per-seat classifications from the power, exit, and role data; this story authors the structure and does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive sits nearest the beneficiary pole (d near 0.0): the arrangement exists to serve its operational freedom, and its arbitrage-grade exit means it can redefine the constraint whenever it binds. Operators are beneficiaries with constrained exit — they collect cover and indemnification but cannot individually alter terms, placing them slightly off the pure-beneficiary pole. Coalition partners are genuinely mixed: they import the framework's cover (benefit) while exposing their own captured personnel to reciprocal status denial (cost), putting them near symmetric. Internees and civilians sit at the target pole (d near 1.0), amplified by trapped exit — neither can leave detention or geography — and by scope: a globally projected arrangement is hard to verify precisely where its extraction occurs, which scales effective extraction further. Suppression is authored as a raw structural property and is deliberately NOT scaled by power or scope; only extractiveness carries the directional and scope modifiers in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The kernel's founding coordination function — mutual restraint protecting persons hors de combat — is exactly what this reading dissolves: restraint becomes contingent on the restraining party's own convenience. What persists is the treaty shell plus the necessity apparatus performing a different function (violence authorization and legitimacy management). The founding_problem_status (contested) crossed with disappearance_verdict (world_rearranges) is the mismatch signature the consumer cross-checks against the computed theater path: an arrangement the world rearranges around, whose founding restraint-function its own reading disavows, is functioning as captured infrastructure rather than as the coordination it nominally is. The tangled_rope claim guards against both mislabels: calling this pure coordination (rope) erases the detainees and civilians who pay through the same structure that coordinates the state; calling it pure extraction (snare) misses the genuine residual function — the framework still channels violence into authorized forms, sustains alliance interoperability, and retains degraded protections that outright abrogation would eliminate. Both halves are load-bearing; neither is cover for the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel geneva_conventions_1949 (reading: security_maximization_reading). What would adopting a sibling reading change structurally?',
    'Comparative instantiation: authoring the sibling stories (humanitarian_ceiling_reading, conditional_reciprocity_reading) and computing their per-seat classifications over the same referent; the disagreement is located in the conditionality of protections.',
    'Under humanitarian_ceiling, the victim set contracts sharply (status denial and necessity overrides become violations rather than operations) and epsilon collapses toward negligible; under conditional_reciprocity, extraction becomes keyed to adversary conduct, shifting directionality for coalition partners and re-authorizing intermediate positions. This story''s classification holds only for the necessity-keyed reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer structure: one of three readings of the GC-1949 kernel; siblings are separate constraints.').

omega_variable(
    necessity_standard_falsifiability,
    'Is ''operational necessity'' an empirically tractable standard that could ever be violated, or an unfalsifiable authorization formula under which any act can be redescribed as necessary?',
    'Audit cases where necessity claims were tested against outcomes: did suspending specific protections produce measurable, attributable security gains, and did any internal review ever find a necessity claim excessive?',
    'If the standard is unfalsifiable in operation, the arrangement''s coordination justification collapses toward cover and the structure drifts toward pure extraction; if tractable, part of the measured extraction is the contested price of a real (if one-sided) decision procedure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_standard_falsifiability, empirical, 'Whether the necessity override is a decision standard or a blanket license.').

omega_variable(
    repudiation_sincerity,
    'Does the official repudiation of specific interrogation techniques alongside persistent detention and targeting practice represent genuine reform of the arrangement, or theatrical maintenance of a degraded constraint?',
    'Track facility closures, internee transfers to ordinary legal processes, and collateral-standard revisions against continuing practice disclosures over the next decade; distinguish technique-level change from framework-level persistence.',
    'Genuine framework-level reform would drive extractiveness and theater down together; theatrical maintenance would show theater rising while extractiveness plateaus — the piton-drift signature for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(repudiation_sincerity, empirical, 'Whether post-contestation policy changes are substantive or performative.').

omega_variable(
    reciprocity_blowback,
    'Does operating the status-denial and necessity-override machinery measurably degrade the treatment of the state''s own captured personnel by adversaries who cite the same precedent?',
    'Compare treatment of the state''s captured forces before and after the arrangement''s consolidation, controlling for adversary identity and conflict intensity; examine adversary legal justifications invoking the precedent.',
    'Significant blowback would mean part of the arrangement''s cost lands on the beneficiary coalition itself, pulling coalition-partner directionality toward symmetric and weakening the framework''s internal support; negligible blowback would confirm the costs are successfully externalized onto the powerless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_blowback, empirical, 'Whether the reading exports harm back onto its own beneficiaries'' personnel.').

omega_variable(
    operator_identity_lock,
    'How deeply are military and intelligence operators'' professional identities fused with the necessity framework, and would internal dissent capacity recover if the fusion broke?',
    'Longitudinal study of officer corps attitudes across cohorts commissioned before and after the framework''s consolidation; track rates of internal legal objection and whistleblower attrition across generations.',
    'Deep identity lock would make the arrangement self-enforcing at low suppression cost (explaining the falling suppression_requirement tail) and mean reform must come from outside the operator class; shallow lock would predict rising internal resistance as cohorts rotate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operator_identity_lock, empirical, 'Professional-identity fusion of operators with the necessity framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__security_maximization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_1949__security_maximization_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t5, geneva_conventions_1949__security_maximization_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement_basis(gene_tr_t5, observed).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_1949__security_maximization_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement_basis(gene_tr_t10, observed).
narrative_ontology:measurement(gene_tr_t15, geneva_conventions_1949__security_maximization_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(gene_tr_t15, observed).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_1949__security_maximization_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t25, geneva_conventions_1949__security_maximization_reading, theater_ratio, 25, 0.45).
narrative_ontology:measurement_basis(gene_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t5, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 5, 0.63).
narrative_ontology:measurement_basis(gene_be_t5, observed).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(gene_be_t10, observed).
narrative_ontology:measurement(gene_be_t15, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 15, 0.76).
narrative_ontology:measurement_basis(gene_be_t15, observed).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 20, 0.8).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t25, geneva_conventions_1949__security_maximization_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(gene_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t5, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 5, 0.67).
narrative_ontology:measurement_basis(gene_su_t5, observed).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement_basis(gene_su_t10, observed).
narrative_ontology:measurement(gene_su_t15, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 15, 0.75).
narrative_ontology:measurement_basis(gene_su_t15, observed).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t25, geneva_conventions_1949__security_maximization_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(gene_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__security_maximization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__security_maximization_reading, geneva_conventions_1949__conditional_reciprocity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'do the Geneva Conventions bind us in asymmetric conflict?' decomposes into three structurally distinct claims differing on the conditionality of protections. humanitarian_ceiling_reading is the upstream claim (the baseline the treaty text asserts on its face); conditional_reciprocity_reading and security_maximization_reading are downstream parasitizations of the same architecture, each preserving the treaty shell while rewriting the conditionality term. Epsilon differs across the family by construction: the ceiling reading constrains state violence nearly absolutely (negligible extraction); the reciprocity reading makes extraction adversary-indexed; this reading makes it self-indexed and effectively unconditional, hence the highest epsilon in the family. Each member links the others via affects_constraints; contamination propagates downstream — erosion of the ceiling reading's credibility is routinely cited as license for the necessity reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
