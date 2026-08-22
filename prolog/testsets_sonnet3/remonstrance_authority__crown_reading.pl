% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Parlementary Remonstrance as Fiscal Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story instantiates the CROWN reading of the contested
 *   remonstrance-authority kernel: the parlements' right to remonstrate
 *   against and withhold registration of royal edicts is read here as a
 *   minoritarian veto that has drifted from a genuine legal-verification
 *   function into an instrument protecting the particularist fiscal
 *   privileges of venal magistrates, office-holders, and exempt orders, at
 *   growing cost to royal fiscal capacity and to unrepresented taxpayers. The
 *   sibling magistrate_reading treats the same procedural right as a
 *   fundamental constitutional safeguard against arbitrary royal innovation;
 *   that is a different constraint, authored separately, with its own ε and
 *   its own beneficiary/victim structure. This story does not average across
 *   the two readings or hedge ε between them — it authors the Crown's
 *   structural relationship to the arrangement on its own terms.
 *
 * KEY AGENTS:
 *   - crown_fiscal_administration: primary target (institutional/constrained) — bears delay and borrowing costs from blocked registration
 *   - royal_treasury: primary target (institutional/trapped) — pays interest and credit-cost consequences directly
 *   - parlementary_magistrates: primary beneficiary (organized/arbitrage) — office values and regional influence protected by successful remonstrance
 *   - regional_privileged_orders: secondary beneficiary (powerful/mobile) — tax exemptions shielded by the veto without direct participation
 *   - unrepresented_taxpaying_subjects: diffuse victim (powerless/trapped) — bears substitute regressive taxation when reform is blocked
 *   - royal_council: agenda_setter (institutional/constrained) — decides whether to negotiate, override, or coerce
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.71).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.58).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Parlementary Remonstrance as Fiscal Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '0497c31b-5a92-4261-9c6b-4ca551715b5e').
narrative_ontology:cs_kernel_codification('0497c31b-5a92-4261-9c6b-4ca551715b5e', distributed).
narrative_ontology:cs_authority_grounding('0497c31b-5a92-4261-9c6b-4ca551715b5e', extraction).
narrative_ontology:cs_interpretation_layer_present('0497c31b-5a92-4261-9c6b-4ca551715b5e').
narrative_ontology:cs_reading_relation('0497c31b-5a92-4261-9c6b-4ca551715b5e', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('0497c31b-5a92-4261-9c6b-4ca551715b5e', foundational, sovereign_fiscal_authority_indivisible).
narrative_ontology:cs_axiom_status(sovereign_fiscal_authority_indivisible, holdable).
narrative_ontology:cs_axiom_grounding('0497c31b-5a92-4261-9c6b-4ca551715b5e', sovereign_fiscal_authority_indivisible, conventional).
narrative_ontology:cs_axiom('0497c31b-5a92-4261-9c6b-4ca551715b5e', secondary, particularist_privilege_lacks_constitutional_standing).
narrative_ontology:cs_axiom_status(particularist_privilege_lacks_constitutional_standing, holdable).
narrative_ontology:cs_axiom_grounding('0497c31b-5a92-4261-9c6b-4ca551715b5e', particularist_privilege_lacks_constitutional_standing, deontological).
narrative_ontology:cs_reference_frame('0497c31b-5a92-4261-9c6b-4ca551715b5e', absolute_sovereign_fiscal_prerogative).
narrative_ontology:cs_drift_state('0497c31b-5a92-4261-9c6b-4ca551715b5e', pre_revolutionary_fiscal_crisis, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('0497c31b-5a92-4261-9c6b-4ca551715b5e', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, parlementary_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_office_holders).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, regional_privileged_orders).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_treasury).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_fiscal_administration).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, unrepresented_taxpaying_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts fiscal edicts (new taxes, forced loans, currency manipulation, venal office sales) needed to fund war and administration. Must submit edicts to regional parlements for registration; when a parlement remonstrates and refuses registration, the edict cannot be enforced in that jurisdiction without a lit de justice or direct royal coercion. Exit from this friction requires either capitulation to magistrate demands, expensive ceremonial force, or piecemeal jurisdiction-by-jurisdiction imposition — all costly relative to simply having the edict registered.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, crown_fiscal_administration, payer,
    institutional, generational, constrained, national).

% Hold venal, heritable judicial offices whose value depends partly on the parlement's power to block or delay royal fiscal measures that would devalue those offices, threaten local tax exemptions, or bypass local elites' consent. Remonstrate against edicts framed as defending 'fundamental law,' while the practical effect is protecting their own office values, regional tax privileges, and the fiscal exemptions of the orders they are drawn from. Can escalate remonstrance repeatedly, forcing the Crown into costly registration ceremonies, and face little personal risk since dismissal or exile is rare and reversible.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, parlementary_magistrates, beneficiary,
    organized, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, parlementary_magistrates, agenda_setter).

% Purchased offices whose resale value and income depend on the parlement successfully resisting reforms that would abolish venality, restructure the tax base, or dilute their privileges. Benefit passively from every successful remonstrance without personally participating in it.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_office_holders, beneficiary,
    organized, generational, arbitrage, regional).

% Nobility and clergy whose tax exemptions and local jurisdictional privileges are the substantive content most remonstrances defend under the language of 'ancient constitution.' They do not sit in the parlement themselves in most cases but benefit whenever a fiscal edict that would touch their exemptions is blocked.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, regional_privileged_orders, beneficiary,
    powerful, generational, mobile, regional).

% Bear the tax burden that remains after privileged orders' exemptions are preserved by successful remonstrance; when the Crown cannot raise revenue efficiently through orderly registration, it falls back on regressive indirect taxes, venal office proliferation, and currency debasement, all of which land disproportionately on commoners. Have no seat in the remonstrance process and no standing to be heard by either Crown or parlement.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, unrepresented_taxpaying_subjects, payer,
    powerless, biographical, trapped, national).

% Bears the direct fiscal cost of delayed or blocked registration: interest accrues on unfunded war debt, credit terms worsen as lenders price in registration risk, and each forced lit de justice consumes political capital that could otherwise be spent on reform. Has no exit from the parlements' jurisdictional monopoly over registration within their regions.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_treasury, payer,
    institutional, immediate, trapped, national).

% Decides whether to negotiate with a remonstrating parlement, override it by lit de justice, or exile/recall magistrates. Each option carries cost: negotiation concedes fiscal ground, override risks provincial unrest and the appearance of tyranny, exile disrupts judicial administration. The council's choices are shaped by how much political capital the Crown can spend at a given moment.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_council, agenda_setter,
    institutional, biographical, constrained, national).

% Local bodies with their own fiscal interests who are neither party to the remonstrance exchange between Crown and parlement nor represented within it, despite bearing consequences of whichever fiscal settlement eventually emerges. Would object that the parlement speaks for magistrate and noble interests, not for the towns or third estate.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_estates_and_towns, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, parlementary_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its cleanest form, the registration-and-remonstrance procedure lets a legal body formally review royal edicts for consistency with existing law before they take effect, catching genuine drafting errors and creating a documented record of royal legislation. That narrow verification function is real and would remain valuable even absent any fiscal dispute.
% TRANSFER_FUNCTION: The remonstrance right, as exercised in ongoing fiscal contests, moves the cost of postponed or blocked taxation from privileged and office-holding elites onto the royal treasury (in borrowing costs and delayed reform) and ultimately onto unrepresented taxpaying subjects (via regressive substitute revenue measures), while preserving the office values and tax exemptions of the magistrates and orders who exercise or benefit from the veto.
% ABSENT_VOICES: Provincial estates, town corporations, and the mass of taxpaying commoners have no procedural role in the registration exchange; they experience its fiscal consequences (debasement, indirect taxes, venal office proliferation) without ever being heard by either side of the Crown-parlement confrontation.
% DISAPPEARANCE_RATIONALE: If the remonstrance veto vanished overnight, the Crown could register fiscal edicts without provincial obstruction, parlementary offices would lose a major source of their political and market value, privileged orders would lose their most reliable institutional shield against tax reform, and royal fiscal administration would face materially lower borrowing costs and faster reform capacity — arrangements throughout the fiscal and judicial system depend on the veto's presence.
% FOUNDING_PROBLEM: The registration procedure was built to give royal edicts formal legal verification and a public record before enforcement, and secondarily to give the Crown's own courts a check against manifestly unlawful commands.
% FOUNDING_PROBLEM_CORROBORATION: Royal jurists and fiscal ministers (an interested party, discounted accordingly) attest the verification function has been supplanted by obstruction. More weight goes to comparative institutional historians examining parlementary finance records showing office resale values tracking remonstrance success rates, and to foreign observers (e.g. contemporary diplomatic correspondence) noting the parlements' fiscal interest in blocking reform independent of any legal defect in the edicts — corroboration from outside both the Crown and the magistrates themselves.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__crown_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__crown_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71 by interval end) because, on the Crown reading, the remonstrance right's practical effect over the recorded interval is to protect magistrate office values and noble/clerical tax exemptions at direct cost to fiscal administration and indirect cost to commoners — not to perform the verification function that would justify low ε. Suppression is moderate (0.58): the Crown retains coercive tools (lit de justice, exile) but each use is costly and politically limited, so the parlements' obstruction is not fully suppressed even under Crown-favorable framing. Accessibility collapse is comparatively low (0.38) because alternative fiscal mechanisms (forced loans, venal office sales, indirect taxation) remain available to the Crown even when registration is blocked — the veto redirects rather than eliminates fiscal options, which is itself part of why its cost lands on unrepresented subjects. Resistance is high (0.74) reflecting the magistrates' persistent, organized, low-personal-risk capacity to remonstrate repeatedly.
 *
 * DIRECTIONALITY LOGIC:
 *   Parlementary magistrates and the office-holders and privileged orders behind them sit near the full-beneficiary end: they collect protected office value and preserved exemptions without bearing the fiscal consequences of blocked reform, and their exit options (arbitrage — they can escalate or moderate remonstrance opportunistically) keep their effective extraction low. The Crown's fiscal administration and treasury sit near the full-target end: trapped by jurisdiction (cannot simply route around a parlement's registration monopoly in its own region) and bearing the direct fiscal cost of delay. Unrepresented taxpaying subjects are also targets, but through a longer causal chain (regressive substitute taxation) rather than direct extraction by the parlement — this is why they are named victims but their situation differs qualitatively from the treasury's.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pair is deliberately set to flag capture on the Crown reading: founding_problem_status is 'dead' (the verification function this procedure was built for has been supplanted) while disappearance_verdict is 'world_rearranges' (the arrangement's removal would still reorganize the fiscal-judicial system). That mismatch is exactly the zombie-mandate signature — an arrangement whose original justification has lapsed but which persists because concentrated beneficiaries (magistrates, office-holders, privileged orders) now depend on it for reasons unrelated to its founding function. The classification as snare on the Crown reading follows from this: the coordination story (legal verification) is cover for what the metrics describe as ongoing asymmetric extraction from Crown and commoners alike.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remonstrance_kernel_reading_divergence,
    'Is the remonstrance right, at its core, a fiscal-legal verification mechanism now captured by particularist interests (Crown reading), or a genuine safeguard of ancient constitutional liberty against royal fiscal innovation (magistrate reading)?',
    'No single empirical test resolves this — it is a live framing dispute in the historiography itself (absolutist vs. constitutionalist historical traditions). Comparative analysis of remonstrance content (procedural/legal objections vs. explicit defense of exemptions and office values) across the historical record could shift the balance of evidence but not settle the normative question of which framing is authoritative.',
    'Adopting the magistrate reading instead would substantially lower authored ε for this same procedural mechanism, move the Crown from the payer set toward the agenda_setter/threat seat, and likely reclassify the arrangement as rope or tangled_rope rather than snare — this is precisely why the two readings are authored as separate constraints rather than reconciled within one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remonstrance_kernel_reading_divergence, conceptual, 'The kernel-level disagreement between crown_reading and magistrate_reading over what the remonstrance right fundamentally is.').

omega_variable(
    venality_causal_weight,
    'How much of the parlements'' remonstrance behavior is causally driven by office-holders'' pecuniary interest in blocking reform, versus genuine (if self-serving) constitutional conviction about the limits of royal fiscal power?',
    'Archival analysis correlating individual magistrates'' remonstrance voting records with their personal office-resale exposure and family exemption holdings; comparison with the small number of magistrates who supported reforms against their own pecuniary interest.',
    'If pecuniary interest dominates, the Crown reading''s snare classification is strongly supported; if constitutional conviction dominates even where it conflicts with self-interest, the coordination function claimed in the magistrate reading gains credibility and the Crown reading''s ε may be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(venality_causal_weight, empirical, 'Whether remonstrance is better explained by pecuniary capture or genuine constitutional principle.').

omega_variable(
    substitute_taxation_incidence,
    'Would blocked fiscal edicts, if registered without obstruction, actually have fallen more equitably than the substitute measures (venal office sales, debasement, indirect taxes) the Crown resorted to instead?',
    'Fiscal-incidence reconstruction comparing the distributional profile of the specific blocked edicts against the actually-enacted substitute revenue measures for the same fiscal years.',
    'If blocked edicts were themselves regressive (e.g. new indirect taxes the parlement opposed on behalf of ordinary subjects, not just elites), the unrepresented_taxpaying_subjects victim assignment weakens and part of the remonstrance function shifts toward genuine, if incidental, taxpayer protection — complicating the pure snare reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitute_taxation_incidence, empirical, 'Whether the specific fiscal measures blocked by remonstrance were themselves more or less regressive than their substitutes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(remo_tr_t20, remonstrance_authority__crown_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement(remo_tr_t40, remonstrance_authority__crown_reading, theater_ratio, 40, 0.31).
narrative_ontology:measurement(remo_tr_t60, remonstrance_authority__crown_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement(remo_tr_t80, remonstrance_authority__crown_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(remo_tr_t100, remonstrance_authority__crown_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(remo_be_t20, remonstrance_authority__crown_reading, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(remo_be_t40, remonstrance_authority__crown_reading, base_extractiveness, 40, 0.53).
narrative_ontology:measurement(remo_be_t60, remonstrance_authority__crown_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement(remo_be_t80, remonstrance_authority__crown_reading, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(remo_be_t100, remonstrance_authority__crown_reading, base_extractiveness, 100, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(remo_su_t20, remonstrance_authority__crown_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(remo_su_t40, remonstrance_authority__crown_reading, suppression_requirement, 40, 0.44).
narrative_ontology:measurement(remo_su_t60, remonstrance_authority__crown_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement(remo_su_t80, remonstrance_authority__crown_reading, suppression_requirement, 80, 0.55).
narrative_ontology:measurement(remo_su_t100, remonstrance_authority__crown_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__crown_reading, 0.06).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, venal_office_system).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, royal_fiscal_credit_regime).

% DUAL FORMULATION NOTE:
% This story and remonstrance_authority__magistrate_reading are the two declared readings of the remonstrance_authority kernel. They share the same underlying legal procedure (registration and remonstrance of royal edicts by sovereign courts) but diverge in ε (this reading authors high extraction from the Crown's fiscal seat; the magistrate reading authors low extraction, framing the arrangement as protective of settled liberty), in beneficiary/victim assignment (this reading names magistrates and privileged orders as beneficiaries and the Crown/unrepresented subjects as victims; the magistrate reading would name the Crown's fiscal innovation as the threat and the body politic as beneficiary of the check), and in claimed_type (snare here, versus rope or tangled_rope there). Both readings are linked here and should be linked reciprocally in the sibling file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
