% ============================================================================
% CONSTRAINT STORY: software_control_legitimacy__freedom_imperative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_control_legitimacy__freedom_imperative_reading, []).

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
 *   constraint_id: software_control_legitimacy__freedom_imperative_reading
 *   human_readable: Proprietary Software Regime, Read as Categorical Denial of User Freedom
 *   domain: software engineering / political economy of technology / intellectual property
 *
 * SUMMARY:
 *   This story instantiates the freedom_imperative reading of the
 *   software_control_legitimacy kernel: the claim that a user's control over
 *   the computing they run is a fundamental entitlement, such that any
 *   license or technical mechanism withholding that control (source access,
 *   modification rights, redistribution rights) is ethically illegitimate
 *   regardless of the commercial or coordination rationale offered for it.
 *   Under this reading, the referent of ε is the standing
 *   proprietary-software arrangement as it actually operates today — not the
 *   free-software alternative this reading endorses. Every proprietary
 *   license, from this reading's own lights, categorically converts its user
 *   into a victim of denied control; the class of 'beneficiary' is not the
 *   vendor (whose authority this reading treats as unearned) but users
 *   considered as rights-holders whose entitlement is vindicated wherever the
 *   reading's premises are accepted, whether or not any individual user is
 *   presently claiming it. This is one of four sibling readings of the same
 *   kernel (commons, pragmatic_openness, property_rights); each is authored
 *   as its own constraint file with its own ε, per the ε-invariance principle
 *   — this file does not average across them or describe the contest
 *   internally.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: agenda_setter (institutional/arbitrage) — hold license and technical control, collect rents
 *   - proprietary_software_users: payer (moderate/constrained) — denied inspection/modification/redistribution rights on tools they depend on
 *   - device_owners_under_locked_platforms: payer (powerless/trapped) — cannot run own-choice software on owned hardware
 *   - downstream_developers_denied_source: payer (moderate/constrained) — blocked from building on or auditing dependencies
 *   - computer_users_as_rights_holders: beneficiary (powerless/trapped) — the class whose standing this reading vindicates
 *   - free_software_movement_advocates: observer/agenda_setter (organized/arbitrage) — articulates and builds the reading's alternative infrastructure
 *   - enterprise_customers_with_support_contracts: excluded (powerful/mobile) — satisfied contractual relationship the reading's frame does not register as counter-evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, 0.81).
domain_priors:suppression_score(software_control_legitimacy__freedom_imperative_reading, 0.62).
domain_priors:theater_ratio(software_control_legitimacy__freedom_imperative_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(software_control_legitimacy__freedom_imperative_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_control_legitimacy__freedom_imperative_reading, snare).
narrative_ontology:human_readable(software_control_legitimacy__freedom_imperative_reading, "Proprietary Software Regime, Read as Categorical Denial of User Freedom").
narrative_ontology:topic_domain(software_control_legitimacy__freedom_imperative_reading, "software engineering / political economy of technology / intellectual property").

domain_priors:requires_active_enforcement(software_control_legitimacy__freedom_imperative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_control_legitimacy__freedom_imperative_reading, 'c478b622-89cb-4b21-a69b-b5456f736d42').
narrative_ontology:cs_kernel_codification('c478b622-89cb-4b21-a69b-b5456f736d42', distributed).
narrative_ontology:cs_authority_grounding('c478b622-89cb-4b21-a69b-b5456f736d42', distributed).
narrative_ontology:cs_reading_relation('c478b622-89cb-4b21-a69b-b5456f736d42', software_control_legitimacy__pragmatic_openness_reading, coexists_with).
narrative_ontology:cs_reading_relation('c478b622-89cb-4b21-a69b-b5456f736d42', software_control_legitimacy__property_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('c478b622-89cb-4b21-a69b-b5456f736d42', software_control_legitimacy__commons_reading, influences).
narrative_ontology:cs_axiom('c478b622-89cb-4b21-a69b-b5456f736d42', foundational, control_over_own_computing_is_fundamental_right).
narrative_ontology:cs_axiom_status(control_over_own_computing_is_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('c478b622-89cb-4b21-a69b-b5456f736d42', control_over_own_computing_is_fundamental_right, deontological).
narrative_ontology:cs_axiom('c478b622-89cb-4b21-a69b-b5456f736d42', secondary, withholding_source_modification_rights_is_illegitimate_regardless_of_consent).
narrative_ontology:cs_axiom_status(withholding_source_modification_rights_is_illegitimate_regardless_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('c478b622-89cb-4b21-a69b-b5456f736d42', withholding_source_modification_rights_is_illegitimate_regardless_of_consent, deontological).
narrative_ontology:cs_reference_frame('c478b622-89cb-4b21-a69b-b5456f736d42', four_freedoms_baseline).
narrative_ontology:cs_drift_state('c478b622-89cb-4b21-a69b-b5456f736d42', platform_lockdown_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c478b622-89cb-4b21-a69b-b5456f736d42', '').
narrative_ontology:cs_kernel_id(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_control_legitimacy__freedom_imperative_reading, computer_users_as_rights_holders).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, device_owners_under_locked_platforms).
narrative_ontology:constraint_victim(software_control_legitimacy__freedom_imperative_reading, downstream_developers_denied_source).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and distribute software under licenses that withhold source code, restrict modification, and use copyright and technical measures (DRM, code signing, remote attestation) to prevent users from inspecting or altering what runs on their own machines. They set license terms, enforce them through litigation and platform gatekeeping, and capture the resulting rents. From this reading's premises, they are the party whose authority over the artifact is illegitimate in the first place — the fact that they built the software does not, in this reading, entitle them to control what a user does with their own computer.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_vendors, agenda_setter,
    institutional, generational, arbitrage, global).

% Run software they cannot inspect, cannot modify to fix bugs or add needed features, and cannot legally share or repair without vendor permission. Many depend on proprietary tools for work, education, or access to essential services and have no practical free-software substitute for their specific need. In this reading, every one of them is a victim of an illegitimate control structure regardless of whether they experience dissatisfaction — the harm is the denial of control itself, not the resulting inconvenience.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, proprietary_software_users, payer,
    moderate, biographical, constrained, global).

% Purchase hardware whose bootloaders, firmware, or operating systems are locked such that they cannot run software of their own choosing on a device they nominally own. Jailbreaking or rooting is often a legal gray zone or a warranty-voiding act. Ownership of the physical object does not confer control of what it computes.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, device_owners_under_locked_platforms, payer,
    powerless, biographical, trapped, global).

% Want to build on, audit, patch, or port software they rely on but are blocked by the absence of source and by licenses that criminalize reverse engineering. They must reimplement functionality from scratch or go without, a direct transfer of labor and opportunity to the original vendor's monopoly on the artifact.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, downstream_developers_denied_source, payer,
    moderate, biographical, constrained, global).

% In this reading, every computer user is a rights-holder whose entitlement to run, study, modify, and share the software running on their own machines is treated as fundamental — a precondition of autonomy over one's own tools, analogous to a civil liberty. This class collectively 'benefits' from the reading's framework in the sense that recognizing the right vindicates their standing, even though most individuals in this class have not organized to claim it and remain practically trapped inside proprietary ecosystems today.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, computer_users_as_rights_holders, beneficiary,
    powerless, civilizational, trapped, global).

% Articulate and campaign for this reading (the four freedoms framework), maintain free-software licenses and projects, and press the ethical argument in public discourse, litigation, and standards bodies. They both diagnose the illegitimacy and build the alternative infrastructure, occupying a dual observer/agenda-setting position relative to the kernel contest itself.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, free_software_movement_advocates, observer,
    organized, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(software_control_legitimacy__freedom_imperative_reading, free_software_movement_advocates, agenda_setter).

% Large organizations that have negotiated favorable proprietary licensing, indemnification, and support SLAs and who experience the arrangement as a functioning commercial relationship, not a rights violation. Their voice — that the arrangement works fine for them and that source access is not what they are purchasing — is largely absent from this reading's framing, which treats the freedom claim as applying uniformly regardless of contractual satisfaction.
narrative_ontology:constraint_stakeholder(software_control_legitimacy__freedom_imperative_reading, enterprise_customers_with_support_contracts, excluded,
    powerful, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_control_legitimacy__freedom_imperative_reading, diffuse).
narrative_ontology:fixing_cost_class(software_control_legitimacy__freedom_imperative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None recognized as legitimate by this reading at the level of control: this reading holds that vendor retention of exclusive control over modification and inspection solves no problem that couldn't be solved, and solved more legitimately, by software released under free licenses. Any coordination benefit proprietary software provides (support guarantees, unified releases, liability allocation) is treated as achievable without denying users the four freedoms, so it does not justify the control structure.
% TRANSFER_FUNCTION: Moves control over computing — the capacity to run, inspect, modify, and redistribute software — from the people who operate the machines to the vendors who wrote and license the code, backed by copyright law, technical restriction measures, and platform gatekeeping.
% ABSENT_VOICES: Enterprise customers and users who report satisfaction with proprietary support relationships are structurally outside this reading's frame, since the reading locates the harm in the denial of control itself rather than in any measurable dissatisfaction; a user's contentment with a proprietary product does not, on this reading's own terms, answer the legitimacy question.
% DISAPPEARANCE_RATIONALE: If proprietary licensing and technical restriction were abolished overnight, free-software advocates hold the software ecosystem would reorganize around openly licensed code with control returned to users; vendors and enterprise customers hold that commercial software development, indemnification, and much current investment-backed innovation would collapse or relocate. The two camps dispute not just the value of the outcome but what would actually happen, which is itself part of the kernel contest this reading is one side of.
% FOUNDING_PROBLEM: Early software culture (pre-1980s, in the free-software movement's own genealogy) treated source sharing and modification as normal collaborative practice; the founding problem this reading identifies is the subsequent enclosure of that practice — the shift to license-and-restrict business models that made running, studying, modifying, and sharing software into acts requiring permission or committing infringement.
% FOUNDING_PROBLEM_CORROBORATION: Free-software movement advocates (the reading's own proponents) attest the problem is live and worsening (locked bootloaders, DRM, SaaS-ification eliminating even nominal ownership). Some corroboration from outside the movement exists in the form of right-to-repair legislative testimony, competition-authority findings on platform lock-in (e.g., app store antitrust inquiries), and computer-science historians documenting the pre-1980s norm shift; property-rights and pragmatic-openness proponents dispute that the shift constitutes a wrong requiring remedy, so corroboration of the problem's existence is broader than corroboration of this reading's remedy.
narrative_ontology:disappearance_verdict(software_control_legitimacy__freedom_imperative_reading, contested).
narrative_ontology:founding_problem_status(software_control_legitimacy__freedom_imperative_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_control_legitimacy__freedom_imperative_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(software_control_legitimacy__freedom_imperative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_control_legitimacy__freedom_imperative_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_control_legitimacy__freedom_imperative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_control_legitimacy__freedom_imperative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(software_control_legitimacy__freedom_imperative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.81 at interval end) because this reading treats the entire proprietary-software transaction as illegitimate transfer of control regardless of price paid or satisfaction reported — the categorical rejection of closed source means every instance of withheld source/modification/redistribution rights counts as extraction under this reading's own premises, not merely instances with measurable grievance. Suppression is authored more moderately (0.62, rising over the interval) because the coercive apparatus — copyright litigation, DRM/DMCA anti-circumvention law, platform code-signing and attestation — has hardened over the software industry's history (from copyright-only enforcement in early decades to today's combined legal-plus-technical lockdown), which the suppression_requirement series traces. Theater ratio is kept low and slowly rising (0.10 to 0.20): from this reading's view there is little performative element to proprietary licensing — the restriction is functionally real and increasingly effective, not mostly theatrical, though EULA 'agreement' rituals and click-through consent add a small and growing theatrical veneer over what is substantively involuntary. All three series share one time grid (T=0,8,16,24,32,40) per the alignment rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary_software_vendors are the agenda-setters who write the license terms and enforce them (institutional power, arbitrage exit — they can restructure their business models freely, unlike their users). Proprietary_software_users, device_owners_under_locked_platforms, and downstream_developers_denied_source are declared victims: the constraint's control transfer runs from them to the vendor, and their exit options range from constrained (moderate power, some substitute products exist) to trapped (powerless device owners with no legal path to modify their own hardware). Computer_users_as_rights_holders is the beneficiary class in a structurally unusual sense — it is not a party collecting rents but the class whose normative standing the reading's argument vindicates; declaring it as beneficiary rather than omitting it keeps the reading's own logic visible (the moral 'payoff' of the reading accrues to this class even where its members are simultaneously listed, under other names, among the payers). This double-appearance (payers who are also, in the abstract, the rights-holder class) is intentional and is the reading's own structure, not an authoring error.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (open, collaborative software practice being enclosed by license-and-restrict business models) is declared live, not dead, which forecloses treating this reading as an obsolete crusade against a settled or already-corrected practice — the reading's proponents and independent right-to-repair/antitrust evidence both attest ongoing enclosure (locked bootloaders, SaaS elimination of ownership). This keeps the classification honest about what the reading is actually doing: naming a persistent, worsening structural condition rather than re-litigating a historical grievance that no longer applies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_freedom_imperative,
    'Is the freedom_imperative reading of software control the structurally correct one, or is it one contestable ethical framing among at least three others (pragmatic_openness, property_rights, commons) that this kernel supports?',
    'No empirical resolution mechanism exists for this question because it is a normative/conceptual dispute about the moral status of control over computing, not an empirical claim about outcomes. What can be tracked is which reading dominates in specific institutional venues over time (legislation, court rulings, standards bodies, developer community norms) as evidence of contested legitimacy rather than settled fact.',
    'If the property_rights_reading is instead taken as structurally correct, the same proprietary licensing arrangement reclassifies with vendors as legitimate beneficiaries and no victim class at all — an entirely different type (likely rope or scaffold) from the same underlying facts. This is the central kernel-contest fact this omega exists to register.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_freedom_imperative, conceptual, 'Which kernel reading (freedom_imperative vs. pragmatic_openness vs. property_rights vs. commons) is structurally correct, if any single one is.').

omega_variable(
    categorical_vs_graduated_harm,
    'Does this reading''s categorical treatment of every proprietary license as harm (regardless of user satisfaction or negotiated terms) correctly capture the moral structure of the situation, or does it erase real variation between coercive lock-in (locked bootloaders) and freely negotiated commercial licensing (enterprise support contracts)?',
    'Compare user/organizational testimony and revealed preference (contract renewal rates, willingness to pay, absence of exit-seeking) across a spectrum from locked consumer hardware to negotiated enterprise licensing; assess whether the reading''s uniform victim classification tracks any observable difference in these populations'' behavior or only tracks the presence/absence of the four freedoms as a formal matter.',
    'If graduated harm is the better model, the extractiveness score authored here (uniform 0.81 across all proprietary software) overstates the harm to satisfied enterprise customers and understates the harm to trapped device owners; a more granular story-family (splitting locked-platform control from ordinary commercial licensing) might be the more accurate decomposition under the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_vs_graduated_harm, conceptual, 'Whether categorical rejection of all proprietary software obscures real variation in coercion versus negotiated consent.').

omega_variable(
    rights_holder_class_without_organized_claim,
    'Can a class (computer_users_as_rights_holders) meaningfully ''benefit'' from a reading''s vindication of their standing when the overwhelming majority of that class has not organized around, or even encountered, the claim being made on their behalf?',
    'Track whether growth in free-software adoption, right-to-repair legislation, and public awareness campaigns correlates with increased self-identification of computer users as rights-holders (survey data, movement membership growth) versus the claim remaining confined to a small activist and developer population.',
    'If the rights-holder class remains almost entirely unaware of or unmobilized around the claim, the ''beneficiary'' designation is doing significant normative work that the structural data (trapped exit options, powerless power level) does not otherwise support — this is a tension internal to the reading worth flagging rather than resolving by fiat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rights_holder_class_without_organized_claim, conceptual, 'Whether an unmobilized, unaware class can coherently occupy the beneficiary role this reading assigns it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_control_legitimacy__freedom_imperative_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t8, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(soft_tr_t16, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(soft_tr_t24, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 24, 0.16).
narrative_ontology:measurement(soft_tr_t32, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 32, 0.18).
narrative_ontology:measurement(soft_tr_t40, software_control_legitimacy__freedom_imperative_reading, theater_ratio, 40, 0.2).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(soft_be_t8, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(soft_be_t16, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(soft_be_t24, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(soft_be_t32, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 32, 0.79).
narrative_ontology:measurement(soft_be_t40, software_control_legitimacy__freedom_imperative_reading, base_extractiveness, 40, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(soft_su_t8, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(soft_su_t16, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 16, 0.49).
narrative_ontology:measurement(soft_su_t24, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(soft_su_t32, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 32, 0.59).
narrative_ontology:measurement(soft_su_t40, software_control_legitimacy__freedom_imperative_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_control_legitimacy__freedom_imperative_reading, identity_coordination).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__pragmatic_openness_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__property_rights_reading).
narrative_ontology:affects_constraint(software_control_legitimacy__freedom_imperative_reading, software_control_legitimacy__commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the software_control_legitimacy kernel, each authored as a separate ε-invariant constraint file per the ε-invariance principle. freedom_imperative_reading is the extraction-maximal reading (categorical rejection of proprietary control, universal victim set among proprietary users). property_rights_reading is expected to be near-inverse (vendors as legitimate beneficiaries, minimal or no victim set). pragmatic_openness_reading treats the question as methodology, likely producing low ε and a rope or absent-conflict classification. commons_reading is the negotiated middle, likely tangled_rope. All four link to each other bidirectionally to preserve the kernel-family structure for contamination/network analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
