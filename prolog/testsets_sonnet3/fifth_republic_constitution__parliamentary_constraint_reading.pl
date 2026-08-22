% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__parliamentary_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__parliamentary_constraint_reading, []).

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
 *   constraint_id: fifth_republic_constitution__parliamentary_constraint_reading
 *   human_readable: Fifth Republic Constitution — Parliamentary Constraint Reading of Executive Authority
 *   domain: constitutional_law/political_systems
 *
 * SUMMARY:
 *   This story instantiates the parliamentary-constraint reading of the Fifth
 *   Republic's kernel text: the president is read as a coordinated executive
 *   whose policy program only becomes binding law through Assembly
 *   authorization, not as a quasi-sovereign figure acting on direct popular
 *   mandate. Under this reading extraction runs low and the beneficiary is
 *   the legislative majority (and, through it, the electorate's separate
 *   mandate), while the president enters the victim set precisely in the
 *   moments — cohabitation, hostile majorities, censure threats — when the
 *   authorization requirement actually bites. This is a distinct constraint
 *   from the hyper_presidential_reading (which treats the same text as
 *   vesting the president with near-plenary authority weakly checked by the
 *   Assembly) and from the cohabitation_equilibrium_reading (which treats
 *   authority as negotiated between two co-equal executive poles rather than
 *   as legislature-gated). The three readings are not the same constraint
 *   measured differently; each authors its own ε and its own
 *   beneficiary/victim structure from the same kernel text, per the
 *   ε-invariance principle.
 *
 * KEY AGENTS:
 *   - national_assembly_majority: primary beneficiary/agenda_setter (institutional/mobile) — holds the authorization and confidence levers
 *   - president: primary target under this reading (powerful/constrained) — bears the cost of legislative gatekeeping when unaligned with the majority
 *   - prime_minister_and_cabinet: dual-positioned (powerful/constrained) — the operative seat when cohabitation forces authority downward from the presidency
 *   - opposition_deputies: secondary beneficiary (organized/mobile) — uses the authorization requirement as leverage even without holding the majority itself
 *   - electorate: diffuse beneficiary (organized/mobile, generational) — retains a check via separately elected Assembly
 *   - constitutional_council: analytical observer — adjudicates the boundary of what counts as authorized executive action
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__parliamentary_constraint_reading, 0.22).
domain_priors:suppression_score(fifth_republic_constitution__parliamentary_constraint_reading, 0.28).
domain_priors:theater_ratio(fifth_republic_constitution__parliamentary_constraint_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fifth_republic_constitution__parliamentary_constraint_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__parliamentary_constraint_reading, rope).
narrative_ontology:human_readable(fifth_republic_constitution__parliamentary_constraint_reading, "Fifth Republic Constitution — Parliamentary Constraint Reading of Executive Authority").
narrative_ontology:topic_domain(fifth_republic_constitution__parliamentary_constraint_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__parliamentary_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__parliamentary_constraint_reading, 'f588b5ac-cfaa-42c2-a353-a1bc150ae158').
narrative_ontology:cs_kernel_codification('f588b5ac-cfaa-42c2-a353-a1bc150ae158', fixed_text).
narrative_ontology:cs_authority_grounding('f588b5ac-cfaa-42c2-a353-a1bc150ae158', practice).
narrative_ontology:cs_interpretation_layer_present('f588b5ac-cfaa-42c2-a353-a1bc150ae158').
narrative_ontology:cs_reading_relation('f588b5ac-cfaa-42c2-a353-a1bc150ae158', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('f588b5ac-cfaa-42c2-a353-a1bc150ae158', fifth_republic_constitution__cohabitation_equilibrium_reading, influences).
narrative_ontology:cs_axiom('f588b5ac-cfaa-42c2-a353-a1bc150ae158', foundational, legislative_confidence_is_binding_constraint).
narrative_ontology:cs_axiom_status(legislative_confidence_is_binding_constraint, holdable).
narrative_ontology:cs_axiom_grounding('f588b5ac-cfaa-42c2-a353-a1bc150ae158', legislative_confidence_is_binding_constraint, conventional).
narrative_ontology:cs_axiom('f588b5ac-cfaa-42c2-a353-a1bc150ae158', secondary, presidential_mandate_subordinate_to_assembly_authorization).
narrative_ontology:cs_axiom_status(presidential_mandate_subordinate_to_assembly_authorization, holdable).
narrative_ontology:cs_axiom_grounding('f588b5ac-cfaa-42c2-a353-a1bc150ae158', presidential_mandate_subordinate_to_assembly_authorization, conventional).
narrative_ontology:cs_reference_frame('f588b5ac-cfaa-42c2-a353-a1bc150ae158', fourth_republic_instability_correction).
narrative_ontology:cs_drift_state('f588b5ac-cfaa-42c2-a353-a1bc150ae158', post_2000_quinquennat_synchronization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f588b5ac-cfaa-42c2-a353-a1bc150ae158', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__parliamentary_constraint_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, electorate_via_legislative_accountability).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president_when_lacking_majority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, opposition_deputies).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__parliamentary_constraint_reading, electorate).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, president).
narrative_ontology:constraint_victim(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, parliamentary_responsibility_doctrine).
narrative_ontology:constraint_vindicates(fifth_republic_constitution__parliamentary_constraint_reading, separation_of_powers_as_control).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls confidence votes, budget approval, and ordinary legislation. Under this reading, the Assembly majority is the seat that must authorize the president's program before it becomes law; it can withhold confidence from the government, amend or reject bills, and — in the extreme — force resignation. It sets the pace and content of policy implementation regardless of presidential preference when it does not share the president's party.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority, beneficiary).

% Proposes national policy direction and appoints the prime minister, but under this reading cannot implement a program without an Assembly majority willing to pass enabling legislation and sustain confidence in the government. When the Assembly is hostile or merely reluctant, the president's initiatives stall, get amended beyond recognition, or die in committee. Dissolution is available but is a costly, uncertain gamble that can produce an even less cooperative Assembly.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, president, payer,
    powerful, biographical, constrained, national).

% Serves at the pleasure of the Assembly's confidence, not merely the president's appointment. Must build and sustain a working majority for the government's program to survive motions of censure. Benefits from acting as the actual locus of implementable policy when it commands a majority, but bears direct removal risk when it does not.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet, payer,
    powerful, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__parliamentary_constraint_reading, prime_minister_and_cabinet, beneficiary).

% Uses censure motions, amendment procedure, and public debate to block or reshape presidential initiatives it opposes. Its structural leverage exists only because implementation requires the Assembly's authorization — this reading is the constitutional resource that gives an electorally weaker minority the capacity to check the executive.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, opposition_deputies, beneficiary,
    organized, biographical, mobile, national).

% Votes separately for president and Assembly, and can produce divided outcomes. Under this reading, that separation is functioning as intended: voters retain a lever (legislative elections) to check executive ambition between presidential terms, and the requirement of legislative authorization is what makes that lever meaningful rather than symbolic.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, electorate, beneficiary,
    organized, generational, mobile, national).

% Adjudicates disputes over whether executive action has exceeded constitutional authorization, including ordinance powers and emergency measures. Its jurisprudence over decades either reinforces or erodes the requirement that implementation flow through legislative channels.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__parliamentary_constraint_reading, constitutional_council, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fifth_republic_constitution__parliamentary_constraint_reading, national_assembly_majority).
narrative_ontology:fixing_cost_class(fifth_republic_constitution__parliamentary_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of unchecked executive policymaking by requiring that presidential initiatives pass through an independently elected Assembly before becoming binding law — coordinating executive proposal with legislative consent so that national policy reflects more than one elected mandate.
% TRANSFER_FUNCTION: Moves effective policymaking authority from the president alone to the president-plus-Assembly-majority pairing; when the pairing is absent, authority shifts toward whichever body — usually the Assembly — controls the legislative and confidence levers, at the president's expense.
% ABSENT_VOICES: Presidential loyalists and constitutional scholars who favor a strong, decisive unitary executive would object that this reading understates presidential authority (Article 16 emergency powers, decree power, foreign affairs domaine réservé); they are represented mainly in the sibling hyper_presidential_reading rather than in this one's own stakeholder set.
% DISAPPEARANCE_RATIONALE: If the legislative-authorization requirement disappeared overnight, the president could implement policy by decree without Assembly consent; censure motions and budget votes would lose their bite; the Assembly majority's leverage would collapse into ceremonial approval, and the constitutional balance would shift decisively toward the hyper-presidential configuration.
% FOUNDING_PROBLEM: The Fourth Republic's parliamentary instability (frequent government collapse, policy paralysis from unstable coalitions) prompted the 1958 Constitution to strengthen the executive — but the framers retained legislative authorization requirements specifically to prevent that strengthened executive from becoming an unchecked plebiscitary presidency.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and Constitutional Council jurisprudence outside the presidency attest the authorization requirement remains functionally live, citing cohabitation periods (1986-88, 1993-95, 1997-2002) where it visibly bound presidential action. Presidents and their administrations, by contrast, have periodically argued the requirement is a residual formality overridden by presidential democratic legitimacy from direct election — a claim this reading treats as contested rather than settled.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__parliamentary_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__parliamentary_constraint_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fifth_republic_constitution__parliamentary_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__parliamentary_constraint_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).
:- end_tests(fifth_republic_constitution__parliamentary_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) because, under this reading, the arrangement is read as functioning coordination — the Assembly's gatekeeping role prevents unilateral executive policymaking rather than extracting rents for a narrow beneficiary. Suppression is moderate (0.28) reflecting the real but bounded coercive weight of confidence votes and legislative blocking power. The measurement series shows extraction and suppression rising during cohabitation periods (1986-88, 1993-95, 1997-2002, centered near 1986 and 2000 in this grid) when the constraint's bite on the president was most visible, then relaxing when presidential and Assembly majorities aligned (2012, post-2002 quinquennat synchronization). Theater ratio stays low-to-moderate throughout; this reading holds that the authorization requirement is substantively enforced rather than performed, though it ticks up around 2012 reflecting scholarly debate over whether synchronized election calendars have hollowed the check into formality.
 *
 * PERSPECTIVAL GAP:
 *   From the Assembly majority's seat, the constraint looks like genuine coordination — it is the mechanism by which two separately elected mandates are reconciled into implementable policy, and the majority is a clear beneficiary. From the president's seat during a hostile or reluctant Assembly, the identical structure computes as extractive constraint on the mandate the president believes was independently conferred by direct election. The engine computes these divergent seat-level types from the shared structural data; this reading's claim is that the Assembly-gating story is descriptively dominant, which is exactly the premise the hyper_presidential_reading denies.
 *
 * DIRECTIONALITY LOGIC:
 *   The national_assembly_majority and, through it, opposition_deputies and the electorate are coded as beneficiaries — the requirement gives them real leverage over policy outcomes, so their derived directionality sits toward the beneficiary end. The president is coded as bearing the cost when Assembly authorization is withheld or conditioned, placing the president's directionality toward the target end in exactly those periods; outside cohabitation, when the president's own party controls the Assembly, the same structural rule is dormant rather than absent, which is why the temporal series shows extraction falling rather than the story declaring the constraint itself as varying in kind.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — Fourth Republic legislative instability — was addressed by strengthening the executive, but this reading holds that the specific mandate to require legislative authorization for implementation was NOT rendered obsolete by that strengthening; it remains live as the mechanism preventing the strengthened executive from becoming unchecked. The founding_problem_status is authored 'contested' rather than 'dead' because whether the requirement still functions as intended, or has been evaded through decree powers and referendum, is itself the live dispute between this reading and the hyper_presidential_reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_requirement_binding_or_formal,
    'Does the legislative-authorization requirement genuinely bind presidential policy implementation, or has it become a formality that presidents route around via decree powers, referenda, and domaine réservé claims (defense, foreign affairs)?',
    'Comparative analysis of legislative outcomes across cohabitation vs. aligned-majority periods: track the rate at which presidential initiatives were substantively amended, blocked, or forced through Article 49.3 without full parliamentary debate.',
    'If the requirement is substantially formal, this reading''s low extraction score is overstated and the hyper_presidential_reading''s account is closer to descriptively accurate for the modern (post-2000 quinquennat) era. If binding, this reading holds even outside cohabitation as an ever-present structural constraint, merely dormant when majorities align.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_requirement_binding_or_formal, empirical, 'Whether Assembly authorization functions as live constraint or residual formality in the synchronized-election era.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the Fifth Republic constitutional text itself genuinely indeterminate between the parliamentary-constraint, hyper-presidential, and cohabitation-equilibrium readings, or does one reading represent the text''s original/dominant meaning with the others as historical drift?',
    'This is the committer-frame question proper: examine whether Constitutional Council jurisprudence, presidential practice, and comparative constitutional scholarship converge on one reading as textually primary, or whether the text was deliberately drafted to be read differently depending on electoral alignment (a design feature, not an interpretive failure).',
    'If one reading is textually dominant, the other two readings (including this one) should be understood as contested departures rather than co-equal structural claims — this would not change this story''s own ε (per the ε-invariance rule, each reading keeps its own authored value) but would change how much weight the corpus should give this reading relative to its siblings when they are compared.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the kernel text has one dominant reading or is genuinely poly-valent by design, and where the disagreement among the three readings is structurally located (in the authorization-versus-emergency-powers balance).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__parliamentary_constraint_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(fift_tr_t1970, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1970, 0.17).
narrative_ontology:measurement(fift_tr_t1986, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 1986, 0.1).
narrative_ontology:measurement(fift_tr_t2000, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fift_tr_t2012, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2012, 0.22).
narrative_ontology:measurement(fift_tr_t2024, fifth_republic_constitution__parliamentary_constraint_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1958, 0.18).
narrative_ontology:measurement(fift_be_t1970, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(fift_be_t1986, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 1986, 0.3).
narrative_ontology:measurement(fift_be_t2000, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2000, 0.28).
narrative_ontology:measurement(fift_be_t2012, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2012, 0.19).
narrative_ontology:measurement(fift_be_t2024, fifth_republic_constitution__parliamentary_constraint_reading, base_extractiveness, 2024, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1958, 0.25).
narrative_ontology:measurement(fift_su_t1970, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1970, 0.24).
narrative_ontology:measurement(fift_su_t1986, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 1986, 0.4).
narrative_ontology:measurement(fift_su_t2000, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(fift_su_t2012, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2012, 0.22).
narrative_ontology:measurement(fift_su_t2024, fifth_republic_constitution__parliamentary_constraint_reading, suppression_requirement, 2024, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__parliamentary_constraint_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__parliamentary_constraint_reading, cohabitation_equilibrium_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposed from the single natural-language label 'the Fifth Republic executive-legislative relationship.' Per the ε-invariance principle, each reading of the fifth_republic_constitution kernel is authored as its own constraint with its own ε, beneficiary/victim structure, and classification: this file (parliamentary_constraint_reading, ε=0.22, rope-claimed, beneficiary=Assembly majority) is linked to hyper_presidential_reading (expected higher presidential-favoring ε structure, president as low-extraction beneficiary rather than victim) and cohabitation_equilibrium_reading (dual-executive negotiated allocation, victim/beneficiary sets split across both executive poles rather than concentrated on the Assembly). The three files do not average into one 'true' ε for the Fifth Republic constitution; they represent structurally distinct claims about the same fixed text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
