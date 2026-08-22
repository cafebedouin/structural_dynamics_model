% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
 *   human_readable: Parlementary Remonstrance Gate over Royal Fiscal Edicts (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   This story instantiates the crown_reading of the remonstrance_authority
 *   kernel: the parlementary remonstrance right as practiced in France,
 *   1715-1771, assessed by the Crown's lights as an illegitimate minoritarian
 *   veto through which a corporation of venal magistrates blocks royal fiscal
 *   legislation to protect particularist privileges — their own tax
 *   exemptions first, those of the privileged orders second. The interval
 *   maps 0=1715 (restoration of remonstrance prior to registration under the
 *   Regency) to 56=1771 (the Maupeou remodelling abolishing the right). The ε
 *   referent is the standing remonstrance arrangement itself, never the
 *   Crown's endorsed alternative of unobstructed registration; values are
 *   reading-indexed over that fixed referent. Per the claim/metric
 *   independence rule, the claimed type (snare — the guardianship story as
 *   cover) and the metrics are authored independently; the engine computes
 *   per-seat classifications from the structural data. The sibling
 *   magistrate_reading is a separate constraint over the same referent and is
 *   not averaged into this file.
 *
 * KEY AGENTS:
 *   - venal_parlement_magistrates: agenda-setting operator and primary beneficiary (organized/identity_locked) — runs the remonstrance gate; its order's fiscal exemptions and office values are the first claims the gate protects
 *   - royal_fiscal_authority: primary target (institutional/constrained) — its fiscal edicts are the objects the gate blocks
 *   - privileged_tax_orders: secondary beneficiary (powerful/arbitrage) — collects exemption protection passively, without operating the gate
 *   - common_taxpayers: diffuse target (powerless/trapped) — bears the concentrated tax burden the blocked reforms would have redistributed; no coalition channel exists (the Estates-General had not met since 1614)
 *   - royal_reform_ministers: targeted agents (powerful/constrained) — careers and programs destroyed by each successful blockage
 *   - provincial_consultative_bodies: excluded alternative consent channel (moderate/trapped)
 *   - legal_historians: analytical observer (analytical/analytical) — reads registers and tax rolls from outside the conflict
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.82).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.78).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, resistance, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Parlementary Remonstrance Gate over Royal Fiscal Edicts (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, 'eb3cd6a8-0a04-4c15-a3e9-171266af36ae').
narrative_ontology:cs_kernel_codification('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', distributed).
narrative_ontology:cs_authority_grounding('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', lineage).
narrative_ontology:cs_interpretation_layer_present('eb3cd6a8-0a04-4c15-a3e9-171266af36ae').
narrative_ontology:cs_reading_relation('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', remonstrance_authority__magistrate_reading, forecloses).
narrative_ontology:cs_axiom('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', foundational, registration_is_delegative_not_constitutive).
narrative_ontology:cs_axiom_status(registration_is_delegative_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', registration_is_delegative_not_constitutive, deontological).
narrative_ontology:cs_axiom('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', foundational, general_interest_licenses_fiscal_override).
narrative_ontology:cs_axiom_status(general_interest_licenses_fiscal_override, holdable).
narrative_ontology:cs_axiom_grounding('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', general_interest_licenses_fiscal_override, instrumental).
narrative_ontology:cs_axiom('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', secondary, venal_office_interests_disqualify_guardianship).
narrative_ontology:cs_axiom_status(venal_office_interests_disqualify_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', venal_office_interests_disqualify_guardianship, empirically_contingent).
narrative_ontology:cs_reference_frame('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', registration_as_sovereign_formality).
narrative_ontology:cs_drift_state('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', eve_of_maupeou_remodelling, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('eb3cd6a8-0a04-4c15-a3e9-171266af36ae', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, venal_parlement_magistrates).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, privileged_tax_orders).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, common_taxpayers).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_reform_ministers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hereditary owners of judicial offices in the sovereign courts of Paris and the provinces. They receive royal edicts from the chancellery, deliberate in closed chambers, and return formal objections (remonstrances) before registering legislation; repeated refusal suspends an edict's force in the courts. Their own order holds broad exemptions from direct taxation, their offices are family property whose market value rests on the courts' authority, and their corporate self-concept is fused with the role of constitutional guardian. Leaving the practice would mean surrendering both the office's value and the order's public identity; the chambers enforced solidarity by suspending sessions and disciplining members who registered against the collective will.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, venal_parlement_magistrates, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(remonstrance_authority__crown_reading, venal_parlement_magistrates, beneficiary).

% The Crown and its financial administration. It drafts fiscal edicts — extensions of the general vingtième, new capitation assessments, reforms of collection — and needs judicial registration for them to bind litigants in the courts. When the chambers remonstrate and refuse registration, its revenue measures are delayed for years or die entirely; its workarounds (forced registration at a lit de justice, exiling hostile chambers, ruling by royal declaration) carry heavy legitimacy costs and provoke wider resistance. It cannot leave the registration system without either dismantling the courts or governing law that no judge will enforce.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_fiscal_authority, payer,
    institutional, generational, constrained, national).

% Nobility, clergy, privileged provinces, and holders of exempted offices and lands. They pay little or no direct taxation and benefit whenever a fiscal edict that would have extended assessments to privileged property is blocked, amended, or withdrawn. They contribute nothing to operating the remonstrance procedure and can shift any residual burden downward; their stake is passive and secure regardless of which constitutional party prevails, so long as exemption itself survives.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, privileged_tax_orders, beneficiary,
    powerful, generational, arbitrage, national).

% Unprivileged commoners — peasants, laborers, merchants, and non-venal townspeople — who bear the direct tax burden concentrated on them by the exemptions the remonstrances defend. They have no seat in the procedure, no access to the chambers, and no channel to contest the assessments that accumulate when reform edicts die; their representation in the process consists of the magistrates invoking the nation's interest on their behalf.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, common_taxpayers, payer,
    powerless, biographical, trapped, national).

% Controllers-general and keepers of the seals who design fiscal reform — Machault's universal vingtième, the war-financing packages of the 1750s, Maupeou's judicial remodelling. Their careers and programs are the immediate objects of remonstrance: a blocked edict ends a ministry's purpose, and forcing registration ends a minister's standing with the courts and the public. They serve at royal pleasure and can be dismissed, but they cannot route their reforms around the registration gate.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, royal_reform_ministers, payer,
    powerful, biographical, constrained, national).

% Provincial estates and would-be consultative assemblies — including the plenary-court and provincial-assembly schemes floated by royal ministers — that could have provided a competing channel of consent and objection for fiscal legislation. The sovereign courts' claim to be the constitution's sole interpreters, backed by their control of registration, kept these bodies marginal or stillborn; the exclusion is renewed every time a fiscal edict is forced through or blocked by the courts alone.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, provincial_consultative_bodies, excluded,
    moderate, generational, trapped, regional).

% Retrospective analysts of the parlementary registers, royal declarations, and fiscal outcomes of the period. They read the remonstrance records, the chancellery's justifications, and the tax rolls from outside the conflict; they collect nothing from the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(remonstrance_authority__crown_reading, legal_historians, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(remonstrance_authority__crown_reading, venal_parlement_magistrates).
narrative_ontology:fixing_cost_class(remonstrance_authority__crown_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The procedure solves the problem of how royal legislation acquires enforceable force across a kingdom of privileged jurisdictions: edicts are communicated to the sovereign courts, whose registration gives them legal effect in litigation, and the courts' formal objection channel gives the judicial corps a structured way to flag conflicts with established law and local custom before enforcement. It also coordinates the fiscal state with the corporations whose cooperation — tax collection, office lending, judicial enforcement — the treasury depended on.
% TRANSFER_FUNCTION: Moves effective control over fiscal legislation from the Crown to a minority corporation of venal magistrates: blocked edicts transfer the costs of fiscal stalemate onto unprivileged taxpayers (whose burden stays concentrated) and onto reform ministries (whose programs die), while transferring security to the privileged orders (whose exemptions survive) and to the magistracy (whose offices and exemptions the gate shields).
% ABSENT_VOICES: The unprivileged taxpayers whose burden the blocked reforms would have redistributed had no voice: no taxpayer sat in the chambers, and the 'nation' whose interest the remonstrances invoked was represented solely by the magistrates themselves. Royal fiscal experts could speak only as the objects of remonstrance, and rival consultative designs — provincial assemblies, plenary courts — were excluded by the courts' claimed monopoly on constitutional interpretation.
% DISAPPEARANCE_RATIONALE: If the remonstrance gate vanished overnight, fiscal edicts would register as of course, the universal vingtième and similar assessments would reach privileged property, the magistracy's corporate authority and office values would collapse, and the political order would reorganize around unobstructed royal legislation — as the 1771 remodelling briefly demonstrated before the 1774 restoration re-created the gate.
% FOUNDING_PROBLEM: Securing the sovereign courts' cooperation in enforcing royal law: registration made edicts binding in litigation, and the remonstrance channel was built to give the courts a formal, non-defiant way to flag errors and conflicts before registering — purchasing judicial buy-in for royal legislation without forcing an open confrontation each time.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the target seat itself — the royal declarations and chancellery justifications of 1770-1771 arguing that judicial cooperation no longer required prior remonstrance — and by the documentary record: the published remonstrance registers of the 1750s-1760s are dominated by defense of fiscal privilege (exempted lands, venal office interests, provincial immunities) rather than by the general constitutional principles invoked in the 1720s-1730s. British and Prussian diplomatic correspondence from the period independently describes the gate's operation in fiscal-blocking terms. The magistracy's own attestation that the founding problem remains live is discounted as self-interested.
narrative_ontology:disappearance_verdict(remonstrance_authority__crown_reading, world_rearranges).
narrative_ontology:founding_problem_status(remonstrance_authority__crown_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(remonstrance_authority__crown_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extraction is high (0.82 at interval end) because the gate transfers fiscal-legislative control to a minority whose own order is the first beneficiary of every blockage, with the rate of transfer wholly decoupled from any service the magistrates render the treasury. Suppression (0.78) is authored as a raw structural property — the gate's coercive hold on royal legislation plus chamber discipline against internal dissent — and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater (0.62) tracks the expansion of guardianship performance — public printed remonstrances, fundamental-law rhetoric, patriotic-party mythology — while, on this reading, the operative function narrowed to privilege defense; the rising series is the rhetoric outgrowing the substance. Accessibility_collapse (0.60): rival consent channels (provincial assemblies, plenary courts) were kept marginal, but the Crown retained forced registration and declaration workarounds, so alternatives narrowed without vanishing. Resistance (0.76) is the target seat's sustained countermeasure program: lit de justice sessions, chamber exiles, the 1766 séance, and finally the 1771 remodelling that abolished the gate outright. The three measurement series share one nine-point grid (0, 7, 14, 21, 28, 35, 42, 49, 56) so no metric is sampled against another's end-state; the trajectories are monotone ratchets, not cycles — each crisis (Unigenitus, the war finances, the Brittany affair) left the gate stronger, an escalation dynamic rather than intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the gate is office, duty, and identity — the magistrates experience remonstrance as the constitution itself, and their identity lock makes the gate's permanence a condition of their selfhood. From the target seats the same gate is a minority's hold on the kingdom's fiscal survival: the Crown experiences each refused registration as confiscation of its legislative authority, ministers as the destruction of their programs, taxpayers as a burden frozen in their disfavor. The powerless seat's coalition potential is structurally absent — no convocation channel, no corporate voice — which is why the taxpayers remain trapped rather than organized despite their numbers. The engine computes these divergent per-seat classifications from role, power, and exit; the divergence between the operator seat's lived rope-like experience and the target seats' lived extraction is the measurement, not a defect to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The magistrates sit near the beneficiary end (d near 0.08): they administer the gate and its protections flow first to their own order. The privileged orders sit near the beneficiary end with arbitrage-grade position: they collect exemption security without operating anything. The Crown and the reform ministers sit near the full-target end: the gate's costs are aimed precisely at their action space, and their exit options (forced registration, dismissal) are costly and delegitimizing rather than arbitrage-grade. Common taxpayers are authored as targets on this reading's own wager — the exemptions the gate defends concentrate the burden on them — but this is the story's most contestable directionality assignment and carries its own omega. One override: the derivation would read the magistracy's identity_locked exit as target-side trapping and push the organized seat's d upward; the override holds d at 0.08 because the lock here binds the operators to the gate's maintenance — they are its beneficiaries and administrators, not its victims. Exit impossibility sustains the constraint rather than victimizing its holders.
 *
 * MANDATROPHY ANALYSIS:
 *   The crown reading holds the founding coordination problem — purchasing judicial buy-in for royal legislation through a formal objection channel — dead: office security and the venal interest already bind the courts, and the gate now operates as privilege defense wearing guardianship dress. The R5 mismatch (status dead × disappearance world_rearranges) flags the arrangement as captured rather than transitional. The classification discipline prevents two errors: reading the gate as pure coordination (the magistrates' own cover story) would miss the asymmetric extraction the registers document; reading it as a transitional scaffold would miss that no sunset exists and none is intended — the operators' identity and office capital depend on its permanence, which is why the 1771 abolition was reversed within three years and why fixing_cost is prohibitive despite the fix being nominally a single coup.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the crown_reading instantiation of the remonstrance_authority kernel — the remonstrance right as illegitimate minoritarian veto protecting particularist privileges. What would the sibling magistrate_reading change structurally, and where exactly is the disagreement located?',
    'The disagreement is located in the coordination/extraction boundary and in who may speak for the nation: the magistrate_reading treats the guardianship function as genuine and royal fiscal innovation as the threat (victims: ancient liberties, provincial particularisms; the Crown as the extracting party), yielding low ε over the same standing arrangement. Resolution requires independent adjudication of the guardianship function''s reality — content analysis of the remonstrance registers and counterfactual fiscal history — not a re-framing of the same file.',
    'Under the sibling reading the same arrangement computes as a constitutional check (rope or tangled_rope) with the Crown in the target seat; per-seat classifications, the victim set, and fixing_cost all invert. The two readings are separate constraints over one referent and must not be averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one reading of the remonstrance_authority kernel; the sibling reading would invert the victim set and ε over the same referent.').

omega_variable(
    guardianship_function_reality,
    'Was the remonstrance right''s constitutional-guardianship function genuine and operative, or was it cover throughout — the crown reading''s central wager?',
    'Systematic content coding of the remonstrance registers 1715-1771: what share of remonstrances articulate general fundamental-law principles versus defend specific fiscal privileges (exempted lands, venal office interests, provincial immunities)? Cross-check with outcomes: did any remonstrance campaign block a measure that harmed the remonstrants'' own order?',
    'If a substantial guardianship share is found, the arrangement is a hybrid with a real coordination function (tangled_rope rather than snare) and ε falls materially; if privilege defense dominates, the snare claim stands and ε rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardianship_function_reality, empirical, 'Whether the constitutional function was real or cover — the crown reading''s core empirical wager.').

omega_variable(
    taxpayer_interest_direction,
    'Did the blocked fiscal reforms actually serve the unprivileged taxpayers (a lower or fairer burden), or did blocking them harm taxpayers by concentrating the burden and destabilizing royal finance?',
    'Fiscal history: distributional analysis of the vingtième assessments as drafted versus as (never) enacted; commoner per-capita burden trajectories against counterfactual reform scenarios; post-1771 and post-1789 burden data.',
    'If blocking harmed taxpayers, common_taxpayers sit at the full-target end and the reading''s general-interest claim holds; if blocking shielded them from heavier royal extraction, they are incidental beneficiaries (d falls), the victim set shrinks, and the crown reading loses its strongest legitimating seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taxpayer_interest_direction, empirical, 'Direction of the taxpayer seat''s structural interest under the gate.').

omega_variable(
    identity_lock_vs_venal_capital,
    'Does the magistracy''s inability to abandon the remonstrance practice reflect identity fusion (guardian self-concept) or rational lock-in to venal office capital whose value depended on the gate?',
    'Behavioral test at the 1771 remodelling: did magistrates who accepted seats in the remodeled courts differ systematically (by office seniority, hereditary position, wealth held outside office, ideological formation) from those who resigned? If defection tracked office economics, capital lock dominates; if it tracked ideological formation, identity fusion dominates.',
    'If capital lock dominates, the constraint''s persistence runs through the venal office market — a purchasable mechanism, since officeholders could be compensated — and fixing_cost drops toward cheap; if identity fusion dominates, exit is constitutively unavailable and the arrangement persists regardless of compensation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_venal_capital, empirical, 'Mechanism of the operator seat''s exit impossibility: identity fusion versus venal capital.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remo_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(remo_tr_t7, remonstrance_authority__crown_reading, theater_ratio, 7, 0.34).
narrative_ontology:measurement(remo_tr_t14, remonstrance_authority__crown_reading, theater_ratio, 14, 0.39).
narrative_ontology:measurement(remo_tr_t21, remonstrance_authority__crown_reading, theater_ratio, 21, 0.43).
narrative_ontology:measurement(remo_tr_t28, remonstrance_authority__crown_reading, theater_ratio, 28, 0.47).
narrative_ontology:measurement(remo_tr_t35, remonstrance_authority__crown_reading, theater_ratio, 35, 0.52).
narrative_ontology:measurement(remo_tr_t42, remonstrance_authority__crown_reading, theater_ratio, 42, 0.56).
narrative_ontology:measurement(remo_tr_t49, remonstrance_authority__crown_reading, theater_ratio, 49, 0.59).
narrative_ontology:measurement(remo_tr_t56, remonstrance_authority__crown_reading, theater_ratio, 56, 0.62).

% Extraction over time
narrative_ontology:measurement(remo_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.56).
narrative_ontology:measurement(remo_be_t7, remonstrance_authority__crown_reading, base_extractiveness, 7, 0.6).
narrative_ontology:measurement(remo_be_t14, remonstrance_authority__crown_reading, base_extractiveness, 14, 0.64).
narrative_ontology:measurement(remo_be_t21, remonstrance_authority__crown_reading, base_extractiveness, 21, 0.67).
narrative_ontology:measurement(remo_be_t28, remonstrance_authority__crown_reading, base_extractiveness, 28, 0.71).
narrative_ontology:measurement(remo_be_t35, remonstrance_authority__crown_reading, base_extractiveness, 35, 0.75).
narrative_ontology:measurement(remo_be_t42, remonstrance_authority__crown_reading, base_extractiveness, 42, 0.78).
narrative_ontology:measurement(remo_be_t49, remonstrance_authority__crown_reading, base_extractiveness, 49, 0.8).
narrative_ontology:measurement(remo_be_t56, remonstrance_authority__crown_reading, base_extractiveness, 56, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(remo_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(remo_su_t7, remonstrance_authority__crown_reading, suppression_requirement, 7, 0.48).
narrative_ontology:measurement(remo_su_t14, remonstrance_authority__crown_reading, suppression_requirement, 14, 0.53).
narrative_ontology:measurement(remo_su_t21, remonstrance_authority__crown_reading, suppression_requirement, 21, 0.57).
narrative_ontology:measurement(remo_su_t28, remonstrance_authority__crown_reading, suppression_requirement, 28, 0.62).
narrative_ontology:measurement(remo_su_t35, remonstrance_authority__crown_reading, suppression_requirement, 35, 0.66).
narrative_ontology:measurement(remo_su_t42, remonstrance_authority__crown_reading, suppression_requirement, 42, 0.7).
narrative_ontology:measurement(remo_su_t49, remonstrance_authority__crown_reading, suppression_requirement, 49, 0.74).
narrative_ontology:measurement(remo_su_t56, remonstrance_authority__crown_reading, suppression_requirement, 56, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, magistrate_reading).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, venal_office_market).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel decomposes into two ε-invariant constraint stories over the same standing arrangement (parlementary remonstrance practice, 1715-1771): this crown_reading authors high ε (the gate as privilege-protecting minoritarian veto; victims: royal fiscal authority, taxpayers, reform ministries) and the sibling magistrate_reading authors low ε (the gate as constitutional check; the Crown as extracting party). The referent is shared and ε is reading-indexed; the readings are separate files linked here, not one constraint with a measurement parameter. This story also couples to venal_office_market, whose office prices both funded the magistracy and were protected by the gate's operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
