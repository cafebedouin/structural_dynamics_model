% ============================================================================
% CONSTRAINT STORY: second_amendment_boundary__militia_conditioned_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_boundary__militia_conditioned_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: second_amendment_boundary__militia_conditioned_reading
 *   human_readable: Militia-Conditioned Reading of the Second Amendment Boundary
 *   domain: constitutional_law/political_theory/firearms_policy
 *
 * SUMMARY:
 *   This story instantiates the militia-conditioned reading of the Second
 *   Amendment kernel: the view that the prefatory clause ('A well regulated
 *   Militia, being necessary to the security of a free State') supplies the
 *   operative scope of the right, such that 'the right of the people to keep
 *   and bear Arms' is protected only in connection with organized militia
 *   service or collective defense, not as a freestanding individual
 *   entitlement. Under this reading, comprehensive firearms regulation is
 *   presumptively constitutional and subject only to modest judicial
 *   scrutiny. The story treats this as one reading of the contested
 *   second_amendment_boundary kernel; the individual_right_reading and
 *   insurrectionist_reading are separate constraints with their own ε,
 *   beneficiary/victim structures, and classifications — this file does not
 *   average across them or describe the contest internally.
 *
 * KEY AGENTS:
 *   - state_and_local_governments: agenda_setter (institutional/analytical) — enacts and enforces regulation premised on this reading
 *   - gun_violence_prevention_advocates: beneficiary (organized/mobile) — benefits from expanded regulatory legitimacy
 *   - individual_gun_owners_in_restrictive_jurisdictions: payer (powerless/constrained) — bears possession restriction
 *   - firearms_collectors: payer (moderate/constrained) — loses independent constitutional anchor for non-militia possession
 *   - self_defense_claimants_in_high_regulation_states: payer (powerless/trapped) — self-defense interest subordinated to militia framing
 *   - national_rifle_association_and_allied_groups: excluded (organized/mobile) — advocates a rival reading not controlling here
 *   - constitutional_historians_and_courts: observer (analytical/analytical) — assesses drafting history and doctrinal status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_boundary__militia_conditioned_reading, 0.42).
domain_priors:suppression_score(second_amendment_boundary__militia_conditioned_reading, 0.5).
domain_priors:theater_ratio(second_amendment_boundary__militia_conditioned_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_boundary__militia_conditioned_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_boundary__militia_conditioned_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_boundary__militia_conditioned_reading, "Militia-Conditioned Reading of the Second Amendment Boundary").
narrative_ontology:topic_domain(second_amendment_boundary__militia_conditioned_reading, "constitutional_law/political_theory/firearms_policy").

domain_priors:requires_active_enforcement(second_amendment_boundary__militia_conditioned_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_boundary__militia_conditioned_reading, '44d44b7d-1b38-424f-8131-2cef2e866ff3').
narrative_ontology:cs_kernel_codification('44d44b7d-1b38-424f-8131-2cef2e866ff3', fixed_text).
narrative_ontology:cs_authority_grounding('44d44b7d-1b38-424f-8131-2cef2e866ff3', lineage).
narrative_ontology:cs_interpretation_layer_present('44d44b7d-1b38-424f-8131-2cef2e866ff3').
narrative_ontology:cs_reading_relation('44d44b7d-1b38-424f-8131-2cef2e866ff3', second_amendment_boundary__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('44d44b7d-1b38-424f-8131-2cef2e866ff3', second_amendment_boundary__insurrectionist_reading, forecloses).
narrative_ontology:cs_axiom('44d44b7d-1b38-424f-8131-2cef2e866ff3', foundational, prefatory_clause_limits_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('44d44b7d-1b38-424f-8131-2cef2e866ff3', prefatory_clause_limits_operative_scope, conventional).
narrative_ontology:cs_axiom('44d44b7d-1b38-424f-8131-2cef2e866ff3', foundational, collective_militia_service_is_the_protected_activity).
narrative_ontology:cs_axiom_status(collective_militia_service_is_the_protected_activity, holdable).
narrative_ontology:cs_axiom_grounding('44d44b7d-1b38-424f-8131-2cef2e866ff3', collective_militia_service_is_the_protected_activity, conventional).
narrative_ontology:cs_axiom('44d44b7d-1b38-424f-8131-2cef2e866ff3', secondary, state_police_power_over_arms_presumptively_valid).
narrative_ontology:cs_axiom_status(state_police_power_over_arms_presumptively_valid, holdable).
narrative_ontology:cs_axiom_grounding('44d44b7d-1b38-424f-8131-2cef2e866ff3', state_police_power_over_arms_presumptively_valid, instrumental).
narrative_ontology:cs_reference_frame('44d44b7d-1b38-424f-8131-2cef2e866ff3', collective_defense_militia_framework).
narrative_ontology:cs_drift_state('44d44b7d-1b38-424f-8131-2cef2e866ff3', post_heller_mcdonald_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('44d44b7d-1b38-424f-8131-2cef2e866ff3', '').
narrative_ontology:cs_kernel_id(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, state_and_local_governments).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies).
narrative_ontology:constraint_beneficiary(second_amendment_boundary__militia_conditioned_reading, urban_communities_seeking_regulation).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, individual_gun_owners_in_restrictive_jurisdictions).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_collectors).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_in_high_regulation_states).
narrative_ontology:constraint_victim(second_amendment_boundary__militia_conditioned_reading, firearms_retailers_and_manufacturers).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, collective_rights_theory_of_second_amendment).
narrative_ontology:constraint_vindicates(second_amendment_boundary__militia_conditioned_reading, prefatory_clause_as_operative_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces firearms regulation — licensing, waiting periods, assault-weapons bans, magazine limits — on the premise that the Second Amendment's scope is bounded by the militia purpose and therefore subject only to rational-basis or intermediate scrutiny rather than the strict protection an unconditioned individual right would demand. Collects legitimacy for a broad regulatory toolkit.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, state_and_local_governments, agenda_setter,
    institutional, generational, analytical, national).

% Organizes politically and litigates on the premise that the prefatory clause narrows the right, which opens legislative space for restriction. Benefits directly from courts and legislatures adopting this reading; has no personal stake in firearms possession being curtailed beyond the policy outcome.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, gun_violence_prevention_advocates, beneficiary,
    organized, generational, mobile, national).

% Administers licensing regimes, background-check systems, and possession restrictions premised on this reading's permissiveness toward regulation; benefits from broader statutory tools to seize, deny, or condition firearm possession, though line officers bear the operational burden of enforcement encounters.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_boundary__militia_conditioned_reading, law_enforcement_agencies, agenda_setter).

% Lives under licensing, registration, and possession restrictions justified by the claim that the constitutional right does not extend to unconditioned individual possession outside militia-type service. Cannot easily relocate without abandoning community, employment, or family ties; bears the restriction as a direct curtailment of a claimed personal right.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, individual_gun_owners_in_restrictive_jurisdictions, payer,
    powerless, biographical, constrained, regional).

% Holds firearms for historical, sporting, or collection purposes disconnected from any militia function; under this reading, such possession has no independent constitutional anchor and is subject to whatever restriction the legislature chooses, exposing collections to registration mandates, bans on certain classes, or confiscation schemes.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_collectors, payer,
    moderate, biographical, constrained, regional).

% Seeks to possess or carry a firearm for personal protection against an immediate threat; under this reading, that interest is not itself constitutionally privileged since the right's core is militia service, so the claimant's access depends entirely on the discretion of licensing authorities, which can be slow, restrictive, or denied outright.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, self_defense_claimants_in_high_regulation_states, payer,
    powerless, immediate, trapped, regional).

% Operates a national market for firearms and ammunition that becomes fragmented and legally exposed under jurisdiction-by-jurisdiction regulation validated by this reading; can shift production and sales emphasis to less-restrictive states but loses uniform national market access and faces compliance costs that smaller firms cannot absorb.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, firearms_retailers_and_manufacturers, payer,
    organized, biographical, mobile, national).

% Advocates for the individual-right reading and is structurally positioned as the opposing interpretive coalition; under a militia-conditioned regime, this coalition's preferred doctrine is not the operative one in the given forum, so its objections are heard as political input but do not control constitutional meaning in that context.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, national_rifle_association_and_allied_groups, excluded,
    organized, generational, mobile, national).

% Analyzes founding-era militia statutes, drafting history, and ratification debates to assess whether the prefatory clause was understood as limiting or merely explanatory. Can shift the operative reading through scholarship and judicial opinion but does not itself bear the practical costs or benefits of any reading.
narrative_ontology:constraint_stakeholder(second_amendment_boundary__militia_conditioned_reading, constitutional_historians_and_courts, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a textual basis for treating firearms possession as a collective, militia-linked activity that democratic bodies can regulate like other public-safety matters, coordinating state and local governments' ability to enact uniform regulatory schemes without constant constitutional invalidation.
% TRANSFER_FUNCTION: Moves interpretive authority over firearms policy from an unconditioned individual-rights baseline to legislative and administrative discretion — shifting the practical capacity to possess, carry, or trade firearms away from individuals in restrictive jurisdictions and toward the state bodies that license and regulate that activity.
% ABSENT_VOICES: Gun-owner advocacy organizations that hold the individual-right or insurrectionist reading are excluded from controlling the interpretive frame in any jurisdiction or court where this reading prevails; they participate in the broader political and legal contest but are not the ones whose textual theory governs outcomes here.
% DISAPPEARANCE_RATIONALE: If courts and legislatures uniformly abandoned the militia-conditioned reading, the entire architecture of firearms licensing, possession restriction, and market regulation premised on 'the right is bounded by militia purpose' would lose its doctrinal foundation; regulatory regimes would need to be rebuilt on narrower means-end-scrutiny grounds compatible with an individual-right baseline, and possession restrictions currently defended by the collective-purpose theory would face much steeper constitutional hurdles.
% FOUNDING_PROBLEM: The reading was developed to reconcile the Second Amendment's prefatory militia clause with a coherent theory of constitutional interpretation that gives operative meaning to every clause of the text, and to preserve state and federal capacity to regulate firearms as a matter of public safety without treating every regulation as a per se constitutional violation.
% FOUNDING_PROBLEM_CORROBORATION: Some historians and jurists outside the gun-control advocacy coalition (including scholars examining founding-era militia statutes and state constitutional analogues) corroborate that collective-defense readings had genuine historical currency prior to Heller (2008); however, the Supreme Court's own majority in District of Columbia v. Heller rejected this reading as the controlling doctrine, so the founding-problem's current 'liveness' as operative constitutional law is contested even among those who find the historical collective-purpose argument credible.
narrative_ontology:disappearance_verdict(second_amendment_boundary__militia_conditioned_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_boundary__militia_conditioned_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_boundary__militia_conditioned_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_boundary__militia_conditioned_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_boundary__militia_conditioned_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_boundary__militia_conditioned_reading_tests).
:- end_tests(second_amendment_boundary__militia_conditioned_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) is moderate: the reading does not itself seize property or income, but it authorizes a real transfer of practical possession-capacity away from individuals and toward state licensing discretion, and that transfer is asymmetric — regulators and advocacy beneficiaries gain policy latitude while gun owners in restrictive jurisdictions, collectors, and self-defense claimants bear concrete access costs. Suppression (0.5) reflects that the reading's operation depends on active enforcement — licensing denials, seizure authority, criminal penalties for unlicensed possession — rather than voluntary coordination; this is a raw structural property and is not scaled by the reading's power or scope. Theater ratio (0.3) is moderate: some enforcement is substantively aimed at public safety, but a portion of compliance apparatus (registration paperwork, permit renewal cycles) functions more as administrative gatekeeping than as a measured safety intervention. Accessibility collapse (0.4) is middling — regulatory alternatives (looser permitting, exemptions) persist in many jurisdictions even where this reading prevails doctrinally, so alternatives have not fully collapsed. Resistance (0.72) is high: this reading meets sustained, well-organized political and litigation resistance from gun-rights coalitions, reflecting that it is a contested doctrinal position, not settled natural law.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (state and local governments), this reading operates as a coordination mechanism — a shared, defensible constitutional basis for public-safety regulation. From the payer seats (individual gun owners, collectors, self-defense claimants), the same doctrine operates as an enforced narrowing of a right they believe protects them personally. The engine computes these divergent seat classifications from the declared power/exit/beneficiary structure; this story does not resolve which seat is 'correct' — that is precisely the kernel contest, routed to the omega variables below rather than argued out in the narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   State and local governments and gun-violence-prevention advocates sit near the beneficiary end: the reading expands their legitimate policy toolkit at no direct personal cost to them. Law enforcement agencies benefit institutionally (expanded authority) while individual officers bear enforcement risk — hence the dual role. Individual gun owners, collectors, and self-defense claimants sit near the target end: the reading directly narrows what they may lawfully possess or carry, and their exit options are constrained or trapped (relocating jurisdictions is costly; the self-defense claimant facing an immediate threat has essentially no exit). Firearms retailers and manufacturers are organized and mobile at the market level (can shift emphasis across states) but still bear compliance fragmentation costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling the prefatory clause with a workable regulatory doctrine — remains genuinely contested rather than resolved or dead: courts, historians, and legislatures actively dispute whether the collective-purpose reading was ever the controlling doctrine or whether it has been superseded by Heller's individual-right holding. Because Heller (2008) expressly rejected this reading in federal constitutional doctrine, the founding_problem_status of 'contested' reflects that the reading survives as a serious minority/dissenting position and in some state constitutional contexts, not as the settled federal rule — avoiding the mislabeling risk of treating either 'this reading is dead' or 'this reading is the settled law' as uncontested fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_operative_or_explanatory,
    'Does the prefatory militia clause function as a legal limitation on the operative clause''s scope, or merely as a non-binding statement of purpose that does not narrow ''the right of the people''?',
    'Comparative analysis of founding-era grammar conventions, contemporaneous state constitutional analogues with similar prefatory-clause structures, and the drafting history of the Second Amendment''s proposed and rejected versions in the First Congress.',
    'If the prefatory clause is found to be purely explanatory and non-limiting, this reading''s core premise collapses and the constraint''s classification shifts toward the individual_right_reading''s structure — beneficiary and victim sets would essentially invert.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_operative_or_explanatory, conceptual, 'Whether the prefatory clause is legally operative or merely explanatory — the central textual dispute underlying this reading.').

omega_variable(
    current_doctrinal_authority_of_reading,
    'Given that District of Columbia v. Heller (2008) and McDonald v. City of Chicago (2010) expressly adopted the individual-right reading as controlling federal constitutional doctrine, to what extent does the militia-conditioned reading retain live legal force versus surviving only as scholarly critique, dissenting opinion, or state-level constitutional argument?',
    'Track post-Heller judicial treatment: instances where courts have applied collective-purpose or militia-conditioned reasoning notwithstanding Heller, state supreme court interpretations of parallel state constitutional arms provisions, and any indication of doctrinal instability or potential future overruling.',
    'If this reading has no live controlling force anywhere, the constraint''s practical extraction and enforcement effects described here would need to be reframed as historical/counterfactual rather than currently operative — significantly lowering the accuracy of the authored extractiveness and suppression values for present-day application.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(current_doctrinal_authority_of_reading, empirical, 'Whether this reading is currently controlling law anywhere or survives only as a minority/historical position post-Heller.').

omega_variable(
    regulatory_beneficiary_capture_risk,
    'Do the state and local governments and law enforcement agencies who benefit from this reading''s regulatory latitude do so purely for the collective-defense/public-safety coordination function the reading is meant to serve, or does the reading also enable agenda-setters to pursue objectives (revenue from licensing fees, administrative discretion, selective enforcement against disfavored groups) unrelated to the coordination rationale?',
    'Empirical study of licensing fee structures, denial-rate disparities across demographic groups, and whether regulatory intensity correlates with actual public-safety outcomes versus administrative or political convenience.',
    'If beneficiary capture is substantial, the tangled_rope classification (genuine coordination function coexisting with asymmetric extraction) is well-supported; if capture is negligible and regulation tracks safety outcomes closely, the reading looks closer to a genuine rope with lower authored extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_beneficiary_capture_risk, empirical, 'Whether regulatory benefit under this reading tracks genuine coordination or extends into unrelated administrative or political extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_boundary__militia_conditioned_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(seco_tr_t8, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement(seco_tr_t16, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(seco_tr_t24, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(seco_tr_t32, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 32, 0.34).
narrative_ontology:measurement(seco_tr_t40, second_amendment_boundary__militia_conditioned_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(seco_be_t8, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 8, 0.34).
narrative_ontology:measurement(seco_be_t16, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(seco_be_t24, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(seco_be_t32, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(seco_be_t40, second_amendment_boundary__militia_conditioned_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(seco_su_t8, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(seco_su_t16, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(seco_su_t24, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(seco_su_t32, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 32, 0.48).
narrative_ontology:measurement(seco_su_t40, second_amendment_boundary__militia_conditioned_reading, suppression_requirement, 40, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_boundary__militia_conditioned_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_boundary__militia_conditioned_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_boundary__militia_conditioned_reading, second_amendment_boundary__insurrectionist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_boundary kernel. individual_right_reading treats the operative clause as establishing a pre-existing personal right unconditioned by the prefatory militia clause (victim set: regulators/public seeking restriction; beneficiary set: individual possessors). insurrectionist_reading treats armed possession as instrumental to potential resistance against tyranny (beneficiary set: potential resistance actors; victim set: state monopoly-on-force interests). This reading (militia_conditioned_reading) treats the prefatory clause as scope-defining, presuming state regulatory legitimacy (beneficiary set: regulators and public-safety advocates; victim set: restricted gun owners). All three share the same constitutional text but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications — per the ε-invariance principle, they are not measurement-parameter variants of one constraint but three separate constraints linked by kernel membership.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
