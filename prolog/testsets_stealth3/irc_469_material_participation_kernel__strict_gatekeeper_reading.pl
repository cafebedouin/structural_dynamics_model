% ============================================================================
% CONSTRAINT STORY: irc_469_material_participation_kernel__strict_gatekeeper_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_irc_469_material_participation_kernel__strict_gatekeeper_reading, []).

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
 *   constraint_id: irc_469_material_participation_kernel__strict_gatekeeper_reading
 *   human_readable: IRC 469 Material Participation Strict Gatekeeper Reading (Verifiable-Labor Documentation Bar)
 *   domain: economic/legal-regulatory
 *
 * SUMMARY:
 *   Section 469 disallows passive activity losses against ordinary income
 *   unless the taxpayer materially participates; this story instantiates the
 *   strict gatekeeper reading of that standard, under which qualifying
 *   participation means verifiable, substantial personal labor proven under a
 *   high documentation bar — contemporaneous records, credible logs,
 *   discounting of reconstructed hour claims. The standing arrangement under
 *   contest is the strict-substantiation regime as administered from 1986 to
 *   the present. This is one file in a constraint family: the colloquial
 *   label 'material participation under IRC 469' decomposes into two
 *   structurally distinct readings of one statutory kernel, and the sibling
 *   file (strategic_shelter_reading) instantiates the permissive-threshold
 *   arrangement with its own epsilon, victim set, and classification. The two
 *   are linked via network.affects_constraints; this file authors only its
 *   own reading. KEY AGENTS (by structural relationship): -
 *   treasury_tax_policy_office: agenda setter (institutional/constrained) —
 *   writes the substantiation standard; could soften it at revenue-scoring
 *   and political cost - irs_examination_function: enforcer with a
 *   beneficiary stake (institutional/constrained) — audit selection, log
 *   demands, disallowance; specialist corps scoped by dispute volume -
 *   us_treasury: primary beneficiary (institutional/constrained) — receives
 *   the fiscal yield of narrowed deductibility -
 *   tax_advisory_documentation_industry: secondary beneficiary
 *   (organized/mobile) — sells the compliance the bar demands -
 *   underdocumented_semiactive_investors: primary target among genuine
 *   participants (powerful/constrained) — real labor, failed proof -
 *   small_landlord_operators: hardest-hit target (moderate/trapped) —
 *   proportional friction exceeds the stakes -
 *   aggressive_shelter_structurers: intended target (powerful/arbitrage) —
 *   bears denial and penalties; migrates to the next gap -
 *   unrepresented_small_claimants: excluded voice (powerless/trapped) —
 *   objects case-by-case, absent from rulemaking -
 *   joint_committee_gao_analysts: analytical observer
 *   (institutional/analytical) — outside evidence on whether the founding
 *   problem persists
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.42).
domain_priors:suppression_score(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.66).
domain_priors:theater_ratio(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(irc_469_material_participation_kernel__strict_gatekeeper_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(irc_469_material_participation_kernel__strict_gatekeeper_reading, tangled_rope).
narrative_ontology:human_readable(irc_469_material_participation_kernel__strict_gatekeeper_reading, "IRC 469 Material Participation Strict Gatekeeper Reading (Verifiable-Labor Documentation Bar)").
narrative_ontology:topic_domain(irc_469_material_participation_kernel__strict_gatekeeper_reading, "economic/legal-regulatory").

domain_priors:requires_active_enforcement(irc_469_material_participation_kernel__strict_gatekeeper_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'dceb0af2-b3c0-412d-8128-0f0797d71f28').
narrative_ontology:cs_kernel_codification('dceb0af2-b3c0-412d-8128-0f0797d71f28', formalized).
narrative_ontology:cs_authority_grounding('dceb0af2-b3c0-412d-8128-0f0797d71f28', lineage).
narrative_ontology:cs_interpretation_layer_present('dceb0af2-b3c0-412d-8128-0f0797d71f28').
narrative_ontology:cs_reading_relation('dceb0af2-b3c0-412d-8128-0f0797d71f28', irc_469_material_participation_kernel__strategic_shelter_reading, forecloses).
narrative_ontology:cs_axiom('dceb0af2-b3c0-412d-8128-0f0797d71f28', foundational, only_verifiable_personal_labor_qualifies).
narrative_ontology:cs_axiom_status(only_verifiable_personal_labor_qualifies, holdable).
narrative_ontology:cs_axiom_grounding('dceb0af2-b3c0-412d-8128-0f0797d71f28', only_verifiable_personal_labor_qualifies, conventional).
narrative_ontology:cs_axiom('dceb0af2-b3c0-412d-8128-0f0797d71f28', foundational, tax_benefits_require_genuine_effort_desert).
narrative_ontology:cs_axiom_status(tax_benefits_require_genuine_effort_desert, holdable).
narrative_ontology:cs_axiom_grounding('dceb0af2-b3c0-412d-8128-0f0797d71f28', tax_benefits_require_genuine_effort_desert, deontological).
narrative_ontology:cs_reference_frame('dceb0af2-b3c0-412d-8128-0f0797d71f28', anti_shelter_strict_substantiation_frame).
narrative_ontology:cs_drift_state('dceb0af2-b3c0-412d-8128-0f0797d71f28', contemporary_short_term_rental_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dceb0af2-b3c0-412d-8128-0f0797d71f28', '').
narrative_ontology:cs_kernel_id(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_examination_function).
narrative_ontology:constraint_beneficiary(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_documentation_industry).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, underdocumented_semiactive_investors).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_landlord_operators).
narrative_ontology:constraint_victim(irc_469_material_participation_kernel__strict_gatekeeper_reading, aggressive_shelter_structurers).
narrative_ontology:constraint_vindicates(irc_469_material_participation_kernel__strict_gatekeeper_reading, strict_substantiation_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and maintains the regulations enumerating the seven material-participation tests and the substantiation expectations attached to them, and decides through guidance and notices how much verification suffices. It could soften the bar by issuing broad safe harbors, but doing so carries revenue-scoring costs and political exposure as a relaxation of anti-abuse policy, so its practical exit from the strict posture is narrow.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, treasury_tax_policy_office, agenda_setter,
    institutional, generational, constrained, national).

% Selects returns for audit, demands contemporaneous hour records, disallows deductions lacking verification, and sustains a specialist examiner corps whose caseload and budget justification track the volume of substantiation disputes. Declining to enforce the standard it is charged with applying is not an available exit; its institutional scope is bound to the bar's friction.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_examination_function, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(irc_469_material_participation_kernel__strict_gatekeeper_reading, irs_examination_function, beneficiary).

% Receives the fiscal yield of narrowed deductibility: every passive loss disallowed for insufficient verification converts investor deduction value into collected revenue, supplemented by accuracy-related penalty assessments. It exits the arrangement only through congressional amendment of the underlying statute.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury, beneficiary,
    institutional, generational, constrained, national).

% Sells the compliance the bar demands: hour-tracking systems, contemporaneous log templates, substantiation packages, grouping-election advice, and audit-defense representation. Fee streams scale with documentation friction; if the bar softened, firms would migrate to adjacent advisory work rather than dissolve, but their current service lines are priced off this constraint.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, tax_advisory_documentation_industry, beneficiary,
    organized, biographical, mobile, national).

% Investors who genuinely spend substantial time on their rental and partnership activities but cannot prove it to the bar's standard: hours reconstructed after the fact, travel and management time discounted, no contemporaneous log kept because none seemed necessary at the time. They absorb disallowed losses and pay for documentation remediation; repositioning capital into REITs or securities is available but forfeits the direct-control investment they deliberately chose.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, underdocumented_semiactive_investors, payer,
    powerful, biographical, constrained, national).

% Own one to a few units, manage them personally, and lack advisory budgets. The documentation bar costs them proportionally more than any deduction at stake, and many respond by simply not claiming losses they may legitimately deserve. Selling the property is the main exit and it is slow, taxed, and often undesirable; holding and absorbing the friction is the default.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, small_landlord_operators, payer,
    moderate, biographical, trapped, regional).

% Design hour-stacking, grouping-election, and short-term-rental strategies aimed at crossing the participation threshold with engineered counts. They bear disallowance, accuracy penalties, and repricing of their products when the bar hardens, and they respond by moving to the next structural gap rather than exiting the domain; their entire product line depends on the gate existing to be crossed.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, aggressive_shelter_structurers, payer,
    powerful, immediate, arbitrage, national).

% Taxpayers who contest disallowance in correspondence audits or pro se Tax Court filings without counsel. They bear the full friction of proving labor they actually performed and have no seat in the notice-and-comment processes where substantiation standards are drafted; their objections surface only as scattered individual case law.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, unrepresented_small_claimants, excluded,
    powerless, immediate, trapped, local).

% Score the revenue effects of proposals to loosen or tighten the participation standard and publish studies of recurring loss-generation vehicles. They sit outside the benefiting parties and supply the external evidence on whether the arrangement's founding problem persists.
narrative_ontology:constraint_stakeholder(irc_469_material_participation_kernel__strict_gatekeeper_reading, joint_committee_gao_analysts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(irc_469_material_participation_kernel__strict_gatekeeper_reading, us_treasury).
narrative_ontology:fixing_cost_class(irc_469_material_participation_kernel__strict_gatekeeper_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Draws a uniform, auditable line between activities whose losses reflect the taxpayer's own substantial labor and activities that are investment positions, so that examiners across districts apply one standard and the wage-and-portfolio tax base is not eroded by paper losses generated without personal effort.
% TRANSFER_FUNCTION: Moves recognized-loss value and compliance spending from individual investors (disallowed deductions, purchased documentation, penalty exposure) to federal revenue and to the tax-advisory and documentation-services sector.
% ABSENT_VOICES: Unrepresented small claimants: pro se Tax Court filers and correspondence-audit respondents object in individual cases but have no seat in the rulemaking record where substantiation standards are written; their experience enters policy only indirectly, through aggregated case law and occasional GAO sampling.
% DISAPPEARANCE_RATIONALE: If the strict substantiation bar vanished overnight, passive losses would flow against wages at scale, Treasury receipts would fall by the scored value of the reopened deductions, shelter products would relaunch around the removed gate, and the documentation-services segment of the advisory industry would contract sharply. The investor-tax compliance economy would reorganize within a few filing seasons.
% FOUNDING_PROBLEM: The pre-1986 shelter industry: mass-marketed real-estate and equipment-leasing partnerships sold to high-bracket professionals to generate paper losses deductible against salaries. Congress enacted section 469 in the Tax Reform Act of 1986 to sever passive losses from ordinary income, and the material-participation standard is the hinge of that severance.
% FOUNDING_PROBLEM_CORROBORATION: Government Accountability Office and Joint Committee on Taxation reporting on recurring loss-generation vehicles (syndicated conservation easements, cost-segregation stacking, short-term-rental hour strategies), and Treasury's own listed-transaction and abusive-avoidance notices, attest from outside the benefiting parties that the shelter problem persists in mutated forms; no source outside the arrangement's defenders attests that the original problem is extinct.
narrative_ontology:disappearance_verdict(irc_469_material_participation_kernel__strict_gatekeeper_reading, world_rearranges).
narrative_ontology:founding_problem_status(irc_469_material_participation_kernel__strict_gatekeeper_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(irc_469_material_participation_kernel__strict_gatekeeper_reading, 'none', 1).
narrative_ontology:epsilon_provenance(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).
:- end_tests(irc_469_material_participation_kernel__strict_gatekeeper_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.42 because the referent is the strict-bar arrangement assessed by this reading's own lights: denials of engineered hour-count claims are counted as the arrangement functioning correctly, not as extraction, so the residual epsilon is the acknowledged collateral cost — genuine participants caught by the documentation bar, plus the documentation spending the bar compels. Suppression is 0.66 as a raw, unscaled structural property: penalty-backed compulsion to document, enforced by a specialist audit corps; alternatives exist (grouping elections, entity routes, REIT placement, simply not claiming), so nothing is trapped at the structural level, but every alternative carries its own bar. Theater is 0.34: most verification activity checks real labor, but a growing share of log production is audit-defense performance — records assembled retrospectively to survive examination rather than to run the business — and the series shows that share climbing. Accessibility collapse is 0.35 because alternatives persist once the bar is understood; resistance is 0.50 because litigation, safe-harbor lobbying, and legislative proposals are constant but never coalition-scale, since payer interests divide (landlords want leniency; compliance sellers are priced off complexity). The three measurement series share one time grid (0, 8, 16, 24, 32, 40) with every metric authored at every point; the suppression_requirement series is authored because the narrative specifically traces enforcement-capacity hardening — consolidation of the contemporaneous-log doctrine, growth of the passive-activity specialist corps, post-2015 audit initiatives — not merely shifting extraction. The claimed type (tangled_rope) is asserted from structure — a real coordination gate plus asymmetric transfer plus active enforcement — independently of the metric values, which are authored descriptively.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats experience the arrangement as the tax system working: a defensible line, uniformly applied, defended against obvious gaming. The payer seats split three ways. Aggressive shelter structurers compute extreme effective extraction from this arrangement, yet this reading counts their denial as the gate operating as designed — the widest divergence between a seat's computed chi and the reading's endorsement of the arrangement sits at that seat. Underdocumented semi-active investors experience the bar as paying twice: once in labor, once in proof. Small landlords experience friction that exceeds the stakes, and rationally stop claiming. Identity-lock operates on the administering professions: examiner specialization fuses career path with the substantiation standard (leniency reads as malpractice from inside that seat), and advisory service lines fuse firm economics with documentation friction (simplification reads as revenue loss). If that professional fusion broke, softened guidance would face less internal resistance than the budget arithmetic alone predicts. Same-level divergence among payers is driven by constraint-specific factors, not global standing: an affluent investor and a small landlord hold similar nominal wealth positions in some cases, but documentation capacity, advisory access, and property liquidity give them different exits from the identical rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations place us_treasury and tax_advisory_documentation_industry at the low-d end; the advisory industry's mobile exit keeps it nearest the arbitrage-grade beneficiary pole, since it collects from the friction without being bound to any particular enforcement posture. irs_examination_function derives mid-low: its agenda-setter position pulls toward administration of the constraint while its secondary beneficiary position (scope and budget justified by dispute volume) pulls toward collection. On the target side, aggressive_shelter_structurers sit near the full-target end — their entire product depends on crossing the gate, and their arbitrage exit moderates the trap but not the extraction rate per engagement. Underdocumented semi-active investors carry high d with constrained exit: they cannot prove their way out and repositioning capital abandons the investment thesis. Small_landlord_operators sit nearest the full-target pole: trapped, lowest power, highest proportional cost. No directionality overrides are used — the role declarations plus exit options derive each seat's position without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live in mutated forms and is corroborated from outside the benefiting parties (GAO and JCT studies of syndicated easements and hour-strategy products; Treasury listed-transaction notices), so this arrangement is not a piton — its gatekeeping function has not atrophied into performance. The tangled_rope classification keeps both halves visible: the bar solves a genuine collective-action problem (one auditable standard protecting a common base) while the same structure transfers value asymmetrically and pays an advisory sector to administer the friction. The danger trajectory is legible in the measurements: theater_ratio climbing from 0.14 to 0.34 signals substantiation activity drifting toward audit-defense performance, and the suppression series shows enforcement capacity ratcheting upward rather than relaxing. If shelter technology were ever fully suppressed while the bar persisted, founding_problem_status would flip to dead against a world_rearranges verdict and the capture/zombie flag would fire — the machinery is positioned to catch that transition, rather than mislabeling a live gate as pure extraction or a captured remnant as coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is the strict_gatekeeper_reading of the irc_469_material_participation_kernel; what structurally changes if the strategic_shelter_reading (permissive threshold achievable through aggressive hour-counting and grouping elections) is adopted instead?',
    'Observe which reading the adjudicating institutions actually apply in contested cases: Tax Court treatment of reconstructed hour logs, scrutiny applied to grouping elections, and whether engineered counts are accepted as qualifying labor.',
    'Adopting the sibling reading widens the qualifying population, collapses compliance friction, raises passive-loss deductibility against ordinary income, and recomputes epsilon and classification for the same statutory text — the two files are different constraints sharing one kernel, not one constraint with two measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: one reading of a contested statutory kernel; the sibling delta is qualifying-population width, compliance friction, and deductibility.').

omega_variable(
    constitutive_criterion_disagreement_location,
    'Is the disagreement between the two readings located in the constitutive criterion of material participation itself (what counts as the taxpayer''s labor) or merely in calibration (how many hours, how much proof)?',
    'Compare the readings'' treatment of edge cases: if both accept the same hour total under different evidence standards, the dispute is calibration; if they classify identical hour profiles differently depending on how the hours were produced, the dispute is constitutive.',
    'Constitutive disagreement supports the forecloses relation declared in cs_structure — no single adjudicating framework can hold both criteria. Mere calibration disagreement would downgrade the relation to influences and make compromise safe harbors stable rather than contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutive_criterion_disagreement_location, conceptual, 'Locates the kernel contest: the readings diverge on what constitutes the taxpayer''s labor, not on threshold size.').

omega_variable(
    antibuse_function_vs_institutional_self_maintenance,
    'Does the strict documentation bar persist because verifiable-labor proof is the only reliable anti-shelter instrument, or because enforcement institutions and the advisory industry have incorporated its friction into their budgets and fee streams?',
    'Natural experiment on safe harbors: where simplified substantiation was offered (short-term-rental hour standards, de minimis rental exceptions), did engineered loss-claiming rise proportionally, or did genuine claimants simply use the harbor while engineered volumes stayed flat?',
    'If safe harbors absorb genuine claimants without reviving shelter-scale losses, a substantial share of the bar''s severity is institutional self-maintenance, and the constraint drifts toward theatrical maintenance — a trend the rising theater_ratio series already registers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antibuse_function_vs_institutional_self_maintenance, empirical, 'Persistence driver: anti-abuse necessity versus enforcement-budget and advisory-fee incorporation.').

omega_variable(
    denial_composition_ambiguity,
    'What fraction of disallowed passive-loss claims are engineered shelter positions (whose denial this reading counts as proper operation) versus genuine-effort taxpayers caught by the documentation bar (whose denial is the reading''s acknowledged collateral cost)?',
    'Audit-quality studies sampling disallowed claims for contemporaneous evidence of real labor, stratified by claim size and vehicle type.',
    'The higher the genuine-effort share, the more this reading''s epsilon understates the extraction experienced inside the arrangement; a dominant engineered share would vindicate the strict bar''s efficiency and push epsilon down.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(denial_composition_ambiguity, empirical, 'Composition of the denied-claim population: engineered positions versus underdocumented genuine participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(irc_469_material_participation_kernel__strict_gatekeeper_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc__tr_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(irc__tr_t0, observed).
narrative_ontology:measurement(irc__tr_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 8, 0.17).
narrative_ontology:measurement_basis(irc__tr_t8, observed).
narrative_ontology:measurement(irc__tr_t16, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(irc__tr_t16, observed).
narrative_ontology:measurement(irc__tr_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(irc__tr_t24, observed).
narrative_ontology:measurement(irc__tr_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 32, 0.29).
narrative_ontology:measurement_basis(irc__tr_t32, observed).
narrative_ontology:measurement(irc__tr_t40, irc_469_material_participation_kernel__strict_gatekeeper_reading, theater_ratio, 40, 0.34).
narrative_ontology:measurement_basis(irc__tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(irc__be_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 0, 0.26).
narrative_ontology:measurement_basis(irc__be_t0, observed).
narrative_ontology:measurement(irc__be_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 8, 0.3).
narrative_ontology:measurement_basis(irc__be_t8, observed).
narrative_ontology:measurement(irc__be_t16, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 16, 0.34).
narrative_ontology:measurement_basis(irc__be_t16, observed).
narrative_ontology:measurement(irc__be_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement_basis(irc__be_t24, observed).
narrative_ontology:measurement(irc__be_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement_basis(irc__be_t32, observed).
narrative_ontology:measurement(irc__be_t40, irc_469_material_participation_kernel__strict_gatekeeper_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement_basis(irc__be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(irc__su_t0, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(irc__su_t0, observed).
narrative_ontology:measurement(irc__su_t8, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(irc__su_t8, observed).
narrative_ontology:measurement(irc__su_t16, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 16, 0.53).
narrative_ontology:measurement_basis(irc__su_t16, observed).
narrative_ontology:measurement(irc__su_t24, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 24, 0.58).
narrative_ontology:measurement_basis(irc__su_t24, observed).
narrative_ontology:measurement(irc__su_t32, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(irc__su_t32, observed).
narrative_ontology:measurement(irc__su_t40, irc_469_material_participation_kernel__strict_gatekeeper_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement_basis(irc__su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(irc_469_material_participation_kernel__strict_gatekeeper_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(irc_469_material_participation_kernel__strict_gatekeeper_reading, irc_469_material_participation_kernel__strategic_shelter_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'material participation under IRC 469' decomposes into two structurally distinct readings of one statutory kernel, per the epsilon-invariance principle. This file instantiates the strict_gatekeeper_reading: qualifying labor must be verifiable and substantial under a high documentation bar, yielding a narrow qualifying population, heavy compliance friction, and rarely-deductible passive losses (epsilon 0.42, assessed by this reading's own lights over the strict-bar arrangement). The sibling file instantiates the strategic_shelter_reading, under which the same statutory text operates as a permissive threshold crossed by engineered hour counts — a different epsilon, a different victim set, a different classification. Each reading cites the other as its foil: enforcement hardening under this reading is the strategic reading's resource condition, and shelter innovation under the sibling reading is this reading's standing justification. The stories are linked through affects_constraints so contamination analysis can trace drift in either direction across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
