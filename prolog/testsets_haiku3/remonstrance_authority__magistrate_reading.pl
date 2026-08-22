% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__magistrate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__magistrate_reading, []).

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
 *   constraint_id: remonstrance_authority__magistrate_reading
 *   human_readable: Parlement Remonstrance Right (Magistrate Reading)
 *   domain: constitutional/political_economy
 *
 * SUMMARY:
 *   The right of remonstrance—the formal constitutional authority of regional
 *   magistracies (Parlements) to object to and delay royal fiscal
 *   edicts—represents a foundational constitutional mechanism in ancien
 *   régime political economy. The magistrate reading treats remonstrance as a
 *   vital check on arbitrary sovereign innovation, preserving ancient
 *   provincial liberties and customary law against Crown absolutism. Under
 *   this reading, the constraint is fundamentally protective: it coordinates
 *   Crown fiscal authority with provincial constitutional voice, ensuring
 *   reforms proceed through negotiated deliberation rather than decree.
 *   However, the structural effect is asymmetric: the magistracy benefits
 *   substantially through tax exemption protection and political leverage;
 *   commoner taxpayers bear the ultimate burden through narrower tax bases;
 *   and the Crown's fiscal authority is constrained. The magistrate reading
 *   claims this asymmetry is justified by the constraint's coordination
 *   function (protecting ancient liberties against arbitrary change), but the
 *   measurement profile (extractiveness rising to 0.68, theater ratio at
 *   0.41, suppression at 0.72) suggests significant extractive overlay. The
 *   constraint is authored as tangled_rope under the magistrate reading:
 *   genuine coordination function (constitutional deliberation, ancient
 *   liberty protection) AND asymmetric extraction (magistrate privilege,
 *   commoner burden). This stands as one coherent reading of the remonstrance
 *   kernel; the crown reading (remonstrance as illegitimate veto protecting
 *   particularist privilege) is a sibling constraint with different ε and
 *   beneficiary structure.
 *
 * KEY AGENTS:
 *   - Provincial magistracy (Parlements): Institutional guardians claiming constitutional authority to protect ancient liberties; exercise remonstrance veto over fiscal edicts; tax-exempt and identity-locked to the remonstrance role
 *   - Crown fiscality: Seeks efficient revenue collection; constrained by magistrate remonstrance and forced into negotiation
 *   - Commoner taxpayers: Powerless, trapped; bear ultimate tax burden because magistracy and nobility are exempt
 *   - Provincial nobility: Tax-exempt beneficiaries riding magistrate protection of 'ancient privilege'
 *   - Crown fiscal reformers: Excluded from remonstrance; advocate for uniform taxation and rationalization
 *   - Enlightenment critics: Excluded; argue remonstrance is tax privilege dressed as constitutional protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__magistrate_reading, 0.72).
domain_priors:theater_ratio(remonstrance_authority__magistrate_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(remonstrance_authority__magistrate_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__magistrate_reading, tangled_rope).
narrative_ontology:human_readable(remonstrance_authority__magistrate_reading, "Parlement Remonstrance Right (Magistrate Reading)").
narrative_ontology:topic_domain(remonstrance_authority__magistrate_reading, "constitutional/political_economy").

domain_priors:requires_active_enforcement(remonstrance_authority__magistrate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__magistrate_reading, '17ed528d-cd93-4d63-9cbf-33b4f12ff7dd').
narrative_ontology:cs_kernel_codification('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', formalized).
narrative_ontology:cs_authority_grounding('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', lineage).
narrative_ontology:cs_interpretation_layer_present('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd').
narrative_ontology:cs_reading_relation('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', remonstrance_authority__crown_reading, coexists_with).
narrative_ontology:cs_axiom('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', foundational, constitutional_precedent_binding_on_sovereignty).
narrative_ontology:cs_axiom_status(constitutional_precedent_binding_on_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', constitutional_precedent_binding_on_sovereignty, deontological).
narrative_ontology:cs_axiom('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', foundational, magistrate_constitutional_guardianship).
narrative_ontology:cs_axiom_status(magistrate_constitutional_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', magistrate_constitutional_guardianship, conventional).
narrative_ontology:cs_reference_frame('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', constitutional_custom_binding).
narrative_ontology:cs_drift_state('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', enlightenment_critique_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('17ed528d-cd93-4d63-9cbf-33b4f12ff7dd', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__magistrate_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_magistracy).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, tax_exempt_nobility).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, crown_fiscality).
narrative_ontology:constraint_victim(remonstrance_authority__magistrate_reading, commoner_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(remonstrance_authority__magistrate_reading, provincial_nobility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The Parlements (regional magistracies) exercise the constitutional right to remonstrate—to formally object and delay royal edicts, particularly fiscal ones. They claim to speak for ancient provincial liberties and the rule of ancient law. This veto power protects their own tax-exempt status and provincial privileges. Their institutional identity is built on being the guardians of constitutional precedent against arbitrary sovereign innovation. Exiting this role means abandoning the identity frame that constitutes their authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_magistracy, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, provincial_magistracy, beneficiary).

% The Crown seeks to raise revenue through fiscal edicts (new taxes, debasement, forced loans). Remonstrance delays and can force negotiation on terms the Magistrates set. The Crown is chronically constrained by the need to negotiate around Parlement resistance rather than simply decree. Revenue collection is slowed; concessions must be made.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_fiscality, payer,
    powerful, biographical, constrained, national).

% Bear the weight of ordinary taxation because the magistracy and nobility are exempt. When Parlements remonstrate, they do so to protect provincial privilege, not commoner welfare. Remonstrance delays may offer temporary relief from new taxes, but ultimately the burden falls back on the non-exempt classes. They have no seat in the remonstrance process and cannot articulate their own interests.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, commoner_taxpayers, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__magistrate_reading, commoner_taxpayers, payer).

% Royal advisors and ministers who seek systematic fiscal reform—rationalization of tax codes, elimination of provincial exemptions, and centralizing revenue. Remonstrance power allows Parlements to block reform edicts in the name of defending 'ancient liberties,' making systematic reform nearly impossible. Reformers are excluded from remonstrance proceedings and can only petition the Crown to override.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_reformers, excluded,
    powerful, biographical, constrained, national).

% Tax-exempt under claimed ancient custom. Remonstrance defends their exemption status by blocking fiscal edicts that would impose uniform taxation. They benefit from the magistracy's use of ancient-liberties framing, which makes their exemption appear to be a constitutional protection rather than naked privilege.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, provincial_nobility, beneficiary,
    powerful, generational, mobile, regional).

% Philosophers and economic critics who argue that remonstrance is an illegitimate veto hiding particularist tax privilege under constitutional language. They argue for rational, uniform taxation and see the 'ancient liberties' claim as a cover for aristocratic extraction. Their writings are circulated but they have no formal seat in governance.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, enlightenment_critics, excluded,
    moderate, biographical, constrained, national).

% The Crown can override remonstrance by issuing lit-de-justice (a formal session where the King commands enactment despite Parlement objection). This power preserves Crown sovereignty but is politically expensive—each override erodes the constitutional legitimacy frame on which the Crown's authority partly rests. The Crown observes the structural constraint and must calculate when to invoke override authority.
narrative_ontology:constraint_stakeholder(remonstrance_authority__magistrate_reading, crown_authority, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__magistrate_reading, provincial_magistracy).
narrative_ontology:fixing_cost_class(remonstrance_authority__magistrate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The remonstrance right enables structured deliberation on fiscal innovation: Parlements can articulate objections, propose amendments, and force the Crown to negotiate. At the coordination level, this prevents arbitrary fiscal action and requires the Crown to justify new edicts in terms of existing constitutional and provincial frameworks. For magistrates it enforces their constitutional seat at the table.
% TRANSFER_FUNCTION: Moves effective veto power over fiscal edicts from the Crown alone to a dual-approval system (Crown intent + Parlement assent, or Crown lit-de-justice override). In practical terms, transfers time, negotiation cost, and concessions from the Crown to the Parlements. The magistracy collects political capital and protection of tax-exempt status; commoners bear the ultimate tax burden; Crown revenue authority is constrained.
% ABSENT_VOICES: Commoner taxpayers, urban merchants, and enlightenment reformers would argue that remonstrance is a mechanism for defending aristocratic tax privilege, not ancient liberties. They are structurally excluded from remonstrance proceedings. Economic critics argue the 'ancient liberty' framing is a cover story for particularist extraction. They have no formal seat.
% DISAPPEARANCE_RATIONALE: If remonstrance authority vanished overnight, the Crown could issue fiscal edicts by royal will alone. Parlement resistance would collapse; provincial exemptions would become vulnerable to rationalization; tax uniformity could advance; magistrate authority would shift from co-legislator to executing agent. The constitutional balance would reorder entirely, making the Crown far more powerful relative to provincial institutions.
% FOUNDING_PROBLEM: Early medieval custom and provincial charters granted provincial magistracies formal consultation rights and blocking authority on matters affecting ancient provincial privilege and liberty. This was understood as a constitutional bulwark against arbitrary sovereign innovation—a check on the Crown's fiscal ambitions and a preservation mechanism for customary law and provincial autonomy.
% FOUNDING_PROBLEM_CORROBORATION: The Parlement magistracy and constitutional historians defending ancient-law doctrine attest the founding problem is live: arbitrary fiscal innovation by the Crown remains a real threat to provincial stability. Crown fiscal reformers and enlightenment critics attest the founding problem has been overtaken by changed economic circumstances and that remonstrance has become a mechanism for defending tax privilege, not ancient liberty. Legislative commissions and economic analyses from outside the magistracy have documented the fiscal impact of magistrate exemptions and the economic case for unified taxation.
narrative_ontology:disappearance_verdict(remonstrance_authority__magistrate_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__magistrate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__magistrate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(remonstrance_authority__magistrate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(remonstrance_authority__magistrate_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__magistrate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__magistrate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__magistrate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the remonstrance right, while providing real coordination benefits, is instrumentally deployed to protect tax exemption—an asymmetric transfer from non-exempt to exempt classes. The measurement series shows extraction rising over the interval (0.48→0.68), consistent with the magistracy accumulating political capital and narrowing the tax base as the constraint matures. Theater ratio (0.41) is moderate: the constitutional language ('ancient liberty,' 'constitutional precedent') is genuinely important to the arrangement's legitimacy, but a growing share of remonstrance activity defends tax exemption rather than broad provincial autonomy. Suppression (0.72) is high because the constraint's persistence depends on actively suppressing alternative readings and crown override authority—the magistracy must maintain the ancient-liberty framing and block reform. Accessibility collapse (0.62) is moderate because alternatives (Crown direct decree, rationalized taxation) are possible but politically expensive; the magistracy's institutional lock makes exit structurally difficult. Resistance (0.71) is high because commoners, reformers, and the Crown all mount sustained objection to remonstrance authority, but none has sufficient power to break it.
 *
 * PERSPECTIVAL GAP:
 *   From the magistrate's seat, remonstrance is a constitutional coordination mechanism—essential protection of ancient provincial liberties and rule-of-law constraints on arbitrary Crown power. From the Crown's seat, remonstrance is a constraining veto power held by a particularist faction, blocking necessary fiscal reform. From commoners' seats, remonstrance protects aristocratic tax privilege while leaving them powerless to articulate their interests. From the Crown-reformer seat, remonstrance is an illegitimate minoritarian veto. The engine computes each seat's classification from the structural data: agenda-setter + beneficiary + identity-lock seats compute toward high-extraction, low-symmetry types; payer + powerless seats compute toward victim/snare-type experience; observer seats remain neutral. The claim (tangled_rope under the magistrate reading) describes genuine coordination plus asymmetric extraction; different seats will experience this as protective legitimacy or extractive veto depending on their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Provincial magistracy: d ≈ 0.15–0.25 (beneficiary, collects tax exemption protection and political leverage, powerful, but identity-locked to the role—exit is institutional death). The identity lock pulls d toward target slightly but the beneficiary status dominates, placing this seat near beneficiary end. Crown fiscality: d ≈ 0.75–0.85 (payer, bears constrained revenue authority, cannot exit the fiscal domain without abandoning sovereignty). Commoner taxpayers: d ≈ 0.80–0.90 (victim, trapped, powerless, excluded from remonstrance, bear the ultimate tax burden). Provincial nobility: d ≈ 0.10–0.20 (beneficiary, tax-exempt, powerful, mobile—could theoretically ally with Crown but choose to ride magistrate protection). The asymmetry is structural: the beneficiary seats have far lower d than the victim seats, driving high effective extraction when scaled by power and scope. Directionality overrides are unnecessary; the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (ancient liberty protection against arbitrary innovation) was live in early development when the remonstrance right was first codified—provincial autonomy and customary law did face Crown encroachment. By the interval measured here (middle to late ancien régime), the founding problem is contested. Crown fiscal reformers argue the problem has been replaced by a new problem (tax inefficiency, aristocratic privilege), and enlightenment critics argue remonstrance has become a mechanism for defending privilege, not liberty. The magistracy maintains the founding-problem framing ('ancient liberties still threatened'), but the evidence of remonstrance deployment (rising extractiveness, focus on exemption protection) supports the critique. The constraint is not yet classified as piton (it retains coordination function and magistrate agency), but mandatrophy signals are present: the founding problem's status is disputed, the constraint's measured extraction is rising, and theater ratio is climbing. A later measurement series might show theater ratio ≥0.50 and might justify piton classification (foundational coordination function substantially replaced by rent-extraction function, persistence by institutional inertia rather than real coordination need). Current data supports tangled_rope with mandatrophy flag: genuine coordination ('ancient liberty' legitimacy does constrain Crown arbitrary action; negotiation is required; reform must be deliberated), but increasingly overlaid with extraction (magistrate privilege, commoner burden, rising selectivity of remonstrance deployment).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ancient_liberty_vs_tax_privilege,
    'Is the magistrate remonstrance right fundamentally a constitutional protection of ancient provincial liberties against arbitrary innovation, or is it functionally a mechanism for defending tax-exempt aristocratic privilege dressed in constitutional language?',
    'Comparative historical analysis of remonstrance deployment: if Parlements remonstrate primarily on matters affecting their exemption status and less on genuinely provincial liberty questions, the privilege reading is supported. If remonstrance defends provincial autonomy broadly and exemption status is secondary, the liberty reading is supported. Document the actual ratio of remonstrance cases by subject matter.',
    'If privilege-primary, the constraint is better classified as snare (extraction via institutional authority); if liberty-primary, tangled_rope remains valid (genuine coordination on constitutional matters, with asymmetric beneficiary/victim structure). The engine''s classification should track the empirical ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ancient_liberty_vs_tax_privilege, empirical, 'Whether remonstrance is genuinely about ancient liberty or instrumentally about tax privilege.').

omega_variable(
    constitutional_precedent_binding,
    'What makes ancient liberties and provincial charters binding on present-day royal authority? Is it the intrinsic normativity of precedent, the contingent legitimacy the Crown grants to historical custom, or the institutional power of the magistracy to enforce the claim?',
    'Textual analysis of Crown and Parlement legal arguments; examination of whether the Crown treats ancient liberties as genuinely binding or as negotiable accommodations. Study instances where the Crown explicitly repudiated remonstrance authority and the consequences—did magistrates accept repudiation, or did constitutional legitimacy erode?',
    'If precedent is binding only by Crown sufferance, the constitutional framing is contingent and the arrangement is snare-like (extraction covered by false constitutional legitimacy). If precedent retains independent normative force, tangled_rope classification holds (genuine coordination on constitutional matters). The binding question directly determines whether the constraint''s persistence is structural or theatrical.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(constitutional_precedent_binding, conceptual, 'Whether ancient constitutional precedent is normatively binding or contingently accommodated.').

omega_variable(
    identity_lock_magistrate_exit,
    'For the provincial magistracy, is exit from the remonstrance role possible, or is institutional identity so fused with constitutional guardianship that abandoning remonstrance authority would mean institutional dissolution?',
    'Historical counterfactual: if a faction of magistrates explicitly renounced remonstrance authority and pledged simple obedience, would the magistracy as an institution survive, or would it cease to be recognizable as a constitutional body? Document cases of magistrate institutional crisis or reformation.',
    'If exit is truly impossible without institutional dissolution, exit_options=''identity_locked'' is correct and directionality is pushed toward full target. If magistrates could theoretically renounce remonstrance and persist as administrative bodies, exit is constrained but not locked, and directionality is less extreme. The identity-lock status affects how much of the measured suppression is structural vs. internalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_magistrate_exit, empirical, 'Whether magistrate institutional identity is fused with remonstrance authority.').

omega_variable(
    kernel_reading_contest,
    'This constraint is the magistrate reading of the remonstrance kernel. The crown reading treats remonstrance as illegitimate minoritarian veto. Which reading accurately describes the standing arrangement''s actual operation and normative status?',
    'Irreducibly contested: the readings differ on foundational normative commitments (whether constitutional precedent is binding, whether ancient liberty is a real doctrine or a cover story). Empirical evidence can document (a) remonstrance deployment patterns, (b) magistrate exemption accumulation, (c) Crown override frequency, (d) broadness of provincial autonomy protection. Both readings will interpret this evidence through their framing, but neither reading can be falsified by data alone because the normative question (''Is precedent binding?'') is not empirical.',
    'If the crown reading is adopted (remonstrance as veto, not coordination), the constraint should be reclassified as snare. If the magistrate reading stands, tangled_rope remains correct. The readings coexist—neither forecloses the other within a single framework; they are held by different parties (magistrates vs. Crown + reformers). This is a preference/legitimacy question, not an empirical one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, preference, 'The fundamental kernel contest: is remonstrance a protective constitutional mechanism or an illegitimate veto?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__magistrate_reading, 0, 32).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__magistrate_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(remo_tr_t8, remonstrance_authority__magistrate_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(remo_tr_t16, remonstrance_authority__magistrate_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(remo_tr_t24, remonstrance_authority__magistrate_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(remo_tr_t32, remonstrance_authority__magistrate_reading, theater_ratio, 32, 0.41).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__magistrate_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(remo_be_t8, remonstrance_authority__magistrate_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(remo_be_t16, remonstrance_authority__magistrate_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(remo_be_t24, remonstrance_authority__magistrate_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(remo_be_t32, remonstrance_authority__magistrate_reading, base_extractiveness, 32, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__magistrate_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(remo_su_t8, remonstrance_authority__magistrate_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(remo_su_t16, remonstrance_authority__magistrate_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(remo_su_t24, remonstrance_authority__magistrate_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(remo_su_t32, remonstrance_authority__magistrate_reading, suppression_requirement, 32, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__magistrate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__magistrate_reading, 0.18).
narrative_ontology:affects_constraint(remonstrance_authority__magistrate_reading, remonstrance_authority__crown_reading).

% DUAL FORMULATION NOTE:
% The remonstrance kernel has two structurally distinct readings: (1) magistrate_reading: remonstrance as protective constitutional mechanism for ancient liberties, benefits the magistracy through tax exemption and constitutional voice, asymmetrically extracts from commoners via magistrate veto. (2) crown_reading: remonstrance as illegitimate veto protecting particularist privilege under constitutional pretense, benefits the magistracy, extracts from both Crown and commoner taxpayers, but classified as pure snare because the 'ancient liberty' coordination function is reinterpreted as institutional cover story. Both readings reference the same standing arrangement (the constitutional remonstrance right); they differ in what that arrangement IS and does. They are linked via network.affects_constraints because the crown reading's acceptance would directly refute the magistrate reading's legitimacy claim—they occupy a single constraint family but emit different constraint classifications under different readings. A consumer must examine the omega variable 'ancient_liberty_vs_tax_privilege' and the measurement evidence (remonstrance deployment patterns, magistrate exemption evolution) to adjudicate between them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
