% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__coordinate_construction_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: constitutional_authority_boundary__coordinate_construction_reading
 *   human_readable: Constitutional Authority Distributed Across Three Co-Equal Branches
 *   domain: constitutional/institutional
 *
 * SUMMARY:
 *   The constitutional design, read through the coordinate construction lens,
 *   establishes three institutional seats (judicial, legislative, executive)
 *   as co-equal interpreters of constitutional meaning within their
 *   respective spheres. No branch claims unilateral final authority; instead,
 *   each interprets the constitution when exercising its powers and contests
 *   rival interpretations through the normal operation of separated powers.
 *   Inter-branch disagreement is not a pathology to be resolved by some final
 *   arbiter—it is the intended friction preventing any single faction from
 *   capturing constitutional meaning. The constraint is claimed as Rope (a
 *   coordination mechanism solving the problem of preventing interpretive
 *   monopoly) while the metrics describe moderate extractiveness and
 *   suppression (the friction and inter-branch conflict necessary to maintain
 *   coordinate interpretation). The claim/metric gap is intentional:
 *   coordinate construction requires constant active defense against
 *   supremacy encroachment.
 *
 * KEY AGENTS:
 *   - Judicial branch: claims to interpret constitution in specific cases but does not monopolize final authority; subject to legislative override and executive non-acquiescence
 *   - Legislative branch: interprets constitution when enacting law; retains amendment power and can override judicial readings through ordinary legislation
 *   - Executive branch: interprets constitution when executing law and implementing discretionary authority; can refuse enforcement of judicial decisions and propose legislative counters
 *   - Citizenry: benefits from redundant checking and friction; loses speed but gains protection against interpretive capture
 *   - Judicial and parliamentary supremacists: structurally excluded from this reading's authority model; appear as competing factions, never as final arbiters
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__coordinate_construction_reading, 0.42).
domain_priors:suppression_score(constitutional_authority_boundary__coordinate_construction_reading, 0.35).
domain_priors:theater_ratio(constitutional_authority_boundary__coordinate_construction_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_authority_boundary__coordinate_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__coordinate_construction_reading, rope).
narrative_ontology:human_readable(constitutional_authority_boundary__coordinate_construction_reading, "Constitutional Authority Distributed Across Three Co-Equal Branches").
narrative_ontology:topic_domain(constitutional_authority_boundary__coordinate_construction_reading, "constitutional/institutional").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__coordinate_construction_reading, '8c4b2104-1193-417b-880a-99af6d2ccb46').
narrative_ontology:cs_kernel_codification('8c4b2104-1193-417b-880a-99af6d2ccb46', formalized).
narrative_ontology:cs_authority_grounding('8c4b2104-1193-417b-880a-99af6d2ccb46', lineage).
narrative_ontology:cs_interpretation_layer_present('8c4b2104-1193-417b-880a-99af6d2ccb46').
narrative_ontology:cs_reading_relation('8c4b2104-1193-417b-880a-99af6d2ccb46', constitutional_authority_boundary__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c4b2104-1193-417b-880a-99af6d2ccb46', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('8c4b2104-1193-417b-880a-99af6d2ccb46', foundational, no_single_final_arbiter).
narrative_ontology:cs_axiom_status(no_single_final_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('8c4b2104-1193-417b-880a-99af6d2ccb46', no_single_final_arbiter, deontological).
narrative_ontology:cs_axiom('8c4b2104-1193-417b-880a-99af6d2ccb46', foundational, inter_branch_contestation_prevents_monopoly).
narrative_ontology:cs_axiom_status(inter_branch_contestation_prevents_monopoly, holdable).
narrative_ontology:cs_axiom_grounding('8c4b2104-1193-417b-880a-99af6d2ccb46', inter_branch_contestation_prevents_monopoly, instrumental).
narrative_ontology:cs_reference_frame('8c4b2104-1193-417b-880a-99af6d2ccb46', distributed_coordinate_interpretation).
narrative_ontology:cs_drift_state('8c4b2104-1193-417b-880a-99af6d2ccb46', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8c4b2104-1193-417b-880a-99af6d2ccb46', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, institutional_separation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, executive_branch).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, citizenry).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__coordinate_construction_reading, coordinate_interpretation_advocates).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, checks_and_balances_doctrine).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__coordinate_construction_reading, constitutional_diffusion_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitution in specific cases brought before it; can invalidate legislative or executive acts within its jurisdiction but cannot enforce compliance beyond its constitutional scope. Participates in authority distribution rather than monopolizing final judgment. Must justify decisions by reference to constitutional text and principle, not by decree.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, judicial_branch, beneficiary).

% Enacts law under constitutional constraint; interprets the constitution when passing legislation and can refuse to appropriate funds or override vetoes. Can amend the constitution directly through prescribed process. Does not defer unilaterally to judicial readings; retains independent interpretive authority within its legislative sphere.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, legislative_branch, beneficiary).

% Executes law and administers government; interprets the constitution when implementing statutes and exercising discretionary authority. Can refuse to enforce judicial decisions deemed unconstitutional (per this reading's logic) and can propose legislation to override prior interpretations. Participates in the distributed authority network.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__coordinate_construction_reading, executive_branch, beneficiary).

% Gains protection through redundant checking: no single branch can unilaterally rewrite constitutional meaning. Benefits from the friction and contestation that distributed authority creates—it prevents captured reading. Bears the cost of slower resolution when branches disagree.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Lawyers, scholars, and political actors who defend the doctrine that coordinate construction prevents any single branch from monopolizing constitutional authority. Win when branches reject supremacy claims from rivals. Lose if one branch successfully claims final interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, coordinate_interpretation_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Would argue that only courts can authoritatively settle constitutional disputes and that legislative override of judicial readings violates the separation of powers. Their framing is structurally excluded from this reading's authority model; their voice appears only in litigation and legislative debate, never as final authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, judicial_supremacy_faction, excluded,
    institutional, generational, mobile, national).

% Would argue that the legislature, being elected and accountable, retains ultimate authority to define the constitution and can override or reverse any judicial or executive reading through ordinary legislation. Their position is structurally excluded from this reading; they appear as political actors and reformers, never as the final arbiter.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, parliamentary_supremacy_faction, excluded,
    institutional, generational, mobile, national).

% The formal mechanism through which the people (via supermajority) can override all three branches and authoritatively settle constitutional disputes. This reading treats amendment as the only true final arbiter, holding all three branches in coordinate tension below it.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__coordinate_construction_reading, constitutional_amendment_process, observer,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(constitutional_authority_boundary__coordinate_construction_reading, constitutional_amendment_process).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__coordinate_construction_reading, diffuse).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__coordinate_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents any single branch from monopolizing constitutional interpretation and rewriting constitutional meaning by fiat. Maintains institutional equilibrium by requiring each branch to justify its readings and exposing disagreement through the normal operation of separated powers.
% TRANSFER_FUNCTION: Distributes interpretive authority (the power to say what the constitution means in specific contexts) across three institutional seats rather than concentrating it in one. The transfer runs from the idea of unified final authority to the practice of distributed, contested authority.
% ABSENT_VOICES: Judicial supremacists and parliamentary supremacists are structurally excluded from this reading's authority model. They appear as political actors advocating for their preferred framing but are never granted final interpretive power under this reading's logic. The people (via amendment) are present but above the three branches rather than within the contestation.
% DISAPPEARANCE_RATIONALE: If the coordinate construction reading vanished—if the three branches accepted judicial supremacy or parliamentary primacy—the power to define constitutional meaning would concentrate, enabling rapid reinterpretation without the friction of inter-branch negotiation. Rights protections would depend on which faction held final authority. The distribution of power would fundamentally shift.
% FOUNDING_PROBLEM: Unified interpretive authority in a single branch concentrates power and enables that branch to rewrite constitutional limits on itself. The Framers sought to prevent tyranny by no single institution monopolizing the power to say what the constitution means.
% FOUNDING_PROBLEM_CORROBORATION: Federalist Papers 47–51 and historical scholarship on separated powers (Rakove, Wood, Sunstein) from outside benefiting factions attest the founding concern. Comparative constitutional law shows the danger empirically: systems with unified final authority in courts (some civil-law jurisdictions) or legislatures (Westminster without entrenchment) exhibit different power distributions than separated systems. The worry remains live in contemporary constitutional debate, attested by constitutional scholars and practitioners across competing readings.
narrative_ontology:disappearance_verdict(constitutional_authority_boundary__coordinate_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_authority_boundary__coordinate_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__coordinate_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__coordinate_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_authority_boundary__coordinate_construction_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).
:- end_tests(constitutional_authority_boundary__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the coordinate construction itself is a constraint on all three branches—each must justify its readings and expose disagreement rather than operating by fiat. The constraint does not disappear disagreement; it institutionalizes it. Suppression is relatively low (0.35) because the mechanism relies on contestation and transparency, not on silencing rival branches—the three branches are expected to disagree and do. Theater ratio is modest (0.28) because much of the inter-branch activity is genuine constitutional negotiation, though some is performative (theatrically invoking separation-of-powers language to defend turf). The measurements show stable operation over the interval: extractiveness and suppression do not drift upward markedly because the coordinate structure is self-regulating—if one branch dominates too visibly, the others resist and the system rebalances. The flatness reflects a working equilibrium, not dormancy.
 *
 * PERSPECTIVAL GAP:
 *   The three institutional seats should experience this constraint differently. The judicial branch experiences coordinate construction as a constraint on its authority (it cannot claim final say) but also as protection (other branches cannot unilaterally override all judicial readings either). The legislative branch experiences it symmetrically: constrained by judicial review but not permanently bound by it; free to amend or override but not to unilaterally redefine without invoking the other branches' resistance. The executive sits in the most ambiguous position—it interprets the constitution when executing but is checked by both other branches. The citizenry experiences the constraint as protection through friction: slower governance but distributed power centers checking each other. The excluded supremacist factions experience it as exclusion: their readings are politically available but structurally denied final authority.
 *
 * DIRECTIONALITY LOGIC:
 *   No stakeholder is a full target (d=1.0) under this reading because the constraint is genuinely distributed—it limits every branch equally. The beneficiary is the arrangement itself (institutional separation, checked power), not a particular institutional seat that captures the gains. Directionality for each institutional seat is near symmetric (d≈0.5) because each bears the constraint (cannot unilaterally redefine meaning) and benefits from it (is checked by but also limits the others). The citizenry is near beneficiary (d≈0.2) because they bear little direct cost and gain protection. The excluded supremacist factions are targets of exclusion (d≈0.8) because the coordinate construction reading actively denies them final authority—but they are not targets of extraction; they are targets of structural foreclosure within this reading's framework.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy would occur if the founding problem (preventing interpretive monopoly) ceased to be live while the coordinate construction remained in place. This happens when one branch de facto monopolizes constitutional interpretation—the Supreme Court claims final say and the other branches acquiesce, or the legislature passes supreme law and courts defer entirely, or the executive exercises unchecked discretion. The constraint persists theatrically but loses its coordination function. The measurements show no drift toward theater-dominant operation over the interval, suggesting the founding problem remains live and inter-branch contestation continues to serve its function. A signal of mandatrophy would be rising theater ratio with flat extractiveness (performing the coordination without actually distributing authority) or rising suppression without contestation (one branch silencing the others rather than negotiating).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordinate_construction_vs_hidden_supremacy,
    'Does coordinate construction genuinely distribute authority, or does one branch de facto monopolize through practice while the others maintain the theoretical frame?',
    'Historical and case-law analysis: do the three branches actually contest constitutional questions and produce substantive disagreement, or do two branches consistently defer to one? Comparative analysis of constitutional systems that formally embrace coordinate construction but exhibit de facto supremacy.',
    'If coordinate construction is performative cover for hidden supremacy, the constraint is closer to a snare (the dominant branch extracts interpretive authority while the ritual of coordinate construction prevents the dominated branches from recognizing or challenging it) rather than a genuine rope. This would raise extractiveness and suppression substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_vs_hidden_supremacy, empirical, 'Whether coordinate construction is structural or theatrical.').

omega_variable(
    inter_branch_conflict_as_extraction,
    'Is inter-branch disagreement a feature of coordinate construction (the intended friction preventing monopoly) or a cost (extractive deadlock that prevents effective governance)?',
    'Policy outcome analysis: when the three branches disagree, do the delayed or compromised outcomes reflect distributed power, or do they represent a deadlock where all three branches lose (negative-sum extraction)? Do citizens gain from the friction or lose from paralysis?',
    'If inter-branch conflict is net-extractive (all branches lose and citizens lose, only the constraint''s abstract principle gains), extractiveness should be reassessed upward and the claimed type reconsidered toward snare. If inter-branch conflict is the intended coordination mechanism, extractiveness remains moderate and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_branch_conflict_as_extraction, conceptual, 'Whether inter-branch contestation is coordination cost or extraction deadlock.').

omega_variable(
    amendment_as_final_arbiter,
    'Does this reading''s claim that no branch has final authority depend on treating the amendment process as the true final arbiter—and does that displace the coordination problem rather than solve it?',
    'Doctrinal analysis: can the amendment process actually function as a final arbiter, or is it so difficult that inter-branch disputes become de facto permanent? Do the three branches cooperate in interpreting whether an amendment succeeds, or do they contest that as well?',
    'If the amendment process cannot practically function as a final arbiter, the constraint lacks a true ultimate authority and inter-branch disputes become unresolvable. This would raise extractiveness and clarify the constraint as institutionalizing ongoing conflict rather than solving it. If amendment can function as a reset mechanism, coordinate construction is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_as_final_arbiter, conceptual, 'Whether the amendment process genuinely breaks inter-branch stalemate or merely defers the conflict.').

omega_variable(
    reading_distinction_and_institutional_identity,
    'Are the three sibling readings (coordinate, judicial-supremacy, parliamentary-primacy) genuinely different constraints, or are they three descriptions of the same constraint viewed from different institutional perspectives?',
    'Structural analysis: do the three readings have different beneficiary/victim structures, different ε values, and different persistence mechanisms? Or do they all describe the same institution (the constitution as a constraint on power) with different framings?',
    'If the readings are genuinely different constraints, each merits its own story with separate ε, stakeholders, and metrics. If they are perspectival readings of one constraint, the ε-invariance principle requires decomposition into separate stories anyway—one per perspective, with explicit omega variables documenting the reading choice. This omega confirms the decomposition as valid (not conflation) or flags it as unnecessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_distinction_and_institutional_identity, conceptual, 'Whether the three readings are distinct constraints or perspectival frames of one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__coordinate_construction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t6, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement_basis(cons_tr_t6, observed).
narrative_ontology:measurement(cons_tr_t12, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(cons_tr_t12, observed).
narrative_ontology:measurement(cons_tr_t18, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement_basis(cons_tr_t18, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_authority_boundary__coordinate_construction_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(cons_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t6, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 6, 0.39).
narrative_ontology:measurement_basis(cons_be_t6, observed).
narrative_ontology:measurement(cons_be_t12, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 12, 0.41).
narrative_ontology:measurement_basis(cons_be_t12, observed).
narrative_ontology:measurement(cons_be_t18, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 18, 0.42).
narrative_ontology:measurement_basis(cons_be_t18, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_authority_boundary__coordinate_construction_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement_basis(cons_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t6, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(cons_su_t6, observed).
narrative_ontology:measurement(cons_su_t12, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 12, 0.34).
narrative_ontology:measurement_basis(cons_su_t12, observed).
narrative_ontology:measurement(cons_su_t18, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 18, 0.35).
narrative_ontology:measurement_basis(cons_su_t18, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 24, 0.35).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_authority_boundary__coordinate_construction_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement_basis(cons_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__coordinate_construction_reading, 0.18).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__coordinate_construction_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel 'constitutional_authority_boundary.' The three sibling readings (coordinate_construction, judicial_supremacy, parliamentary_primacy) constitute a constraint family with different ε values and beneficiary structures. They coexist in live political debate; no single reading has achieved exclusive institutional recognition. The ε-invariance principle required decomposition: a single constraint cannot have three different beneficiary structures. Each reading is a separate constraint with its own story, and they are linked via this network block to enable analysis of the kernel as a whole.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__coordinate_construction_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
