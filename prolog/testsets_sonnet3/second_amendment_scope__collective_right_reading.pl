% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__collective_right_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: second_amendment_scope__collective_right_reading
 *   human_readable: Second Amendment as State-Militia Authority (Collective Right Reading)
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This story instantiates the collective-right reading of the Second
 *   Amendment kernel: the clause is read as protecting state authority to
 *   maintain organized militias, not an individual entitlement to own
 *   firearms. Under this reading the prefatory clause ('A well regulated
 *   Militia...') controls the operative clause's scope, so the right attaches
 *   to the states' collective military organization rather than to persons.
 *   This reading dominated federal appellate consensus (e.g., United States
 *   v. Miller-era interpretation) for much of the 20th century before
 *   District of Columbia v. Heller (2008) displaced it at the Supreme Court
 *   level with an individual-right reading. ε is authored low here because,
 *   under this reading's own terms, the constraint's institutional scope is
 *   narrow (state militia authority) and does not reach into individual
 *   conduct at all — it does not extract from individual gun owners because
 *   it does not purport to govern them; the friction visible in the
 *   historical record belongs to the contest between readings, not to this
 *   reading's internal operation.
 *
 * KEY AGENTS:
 *   - state_governments: primary beneficiary of preserved regulatory latitude (institutional/analytical)
 *   - organized_militia_institutions: the historical referent class this reading protects (organized/analytical)
 *   - individual_gun_owners: excluded from rights-holder class under this reading (moderate/constrained)
 *   - federal_regulatory_bodies: secondary beneficiary via preserved regulatory space (institutional/analytical)
 *   - gun_rights_advocacy_organizations: excluded, forced to pursue other legal theories (organized/mobile)
 *   - constitutional_scholars: analytical observers of ratification-era evidence (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__collective_right_reading, 0.18).
domain_priors:suppression_score(second_amendment_scope__collective_right_reading, 0.22).
domain_priors:theater_ratio(second_amendment_scope__collective_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(second_amendment_scope__collective_right_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_scope__collective_right_reading, "Second Amendment as State-Militia Authority (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_scope__collective_right_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__collective_right_reading, '9b3c12de-1990-4370-bab3-5d86bb999aa9').
narrative_ontology:cs_kernel_codification('9b3c12de-1990-4370-bab3-5d86bb999aa9', fixed_text).
narrative_ontology:cs_authority_grounding('9b3c12de-1990-4370-bab3-5d86bb999aa9', lineage).
narrative_ontology:cs_interpretation_layer_present('9b3c12de-1990-4370-bab3-5d86bb999aa9').
narrative_ontology:cs_reading_relation('9b3c12de-1990-4370-bab3-5d86bb999aa9', second_amendment_scope__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('9b3c12de-1990-4370-bab3-5d86bb999aa9', second_amendment_scope__civic_right_reading, coexists_with).
narrative_ontology:cs_axiom('9b3c12de-1990-4370-bab3-5d86bb999aa9', foundational, prefatory_clause_limits_operative_clause).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('9b3c12de-1990-4370-bab3-5d86bb999aa9', prefatory_clause_limits_operative_clause, conventional).
narrative_ontology:cs_axiom('9b3c12de-1990-4370-bab3-5d86bb999aa9', foundational, right_holder_is_state_militia_not_individual).
narrative_ontology:cs_axiom_status(right_holder_is_state_militia_not_individual, holdable).
narrative_ontology:cs_axiom_grounding('9b3c12de-1990-4370-bab3-5d86bb999aa9', right_holder_is_state_militia_not_individual, conventional).
narrative_ontology:cs_reference_frame('9b3c12de-1990-4370-bab3-5d86bb999aa9', founding_era_militia_federalism).
narrative_ontology:cs_drift_state('9b3c12de-1990-4370-bab3-5d86bb999aa9', post_heller_doctrinal_era, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('9b3c12de-1990-4370-bab3-5d86bb999aa9', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__collective_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, organized_militia_institutions).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federal_preemption_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_scope__collective_right_reading, federal_regulatory_bodies).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, federalism_balance_of_arms_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__collective_right_reading, state_police_power_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states retain broad authority to organize, arm, and regulate militia forces (and by extension civilian firearms generally) without the Second Amendment constraining their regulatory choices at the individual level. States can pass restrictive gun laws without triggering a constitutional individual-rights challenge under this clause.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, state_governments, beneficiary,
    institutional, generational, analytical, national).

% Historical and residual institutions (state militias, National Guard predecessors) whose collective arming function is what the clause is read to protect. Their existence and prerogatives are the amendment's referent; they do not claim any individual entitlement flowing from it.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, organized_militia_institutions, beneficiary,
    organized, generational, analytical, national).

% Under this reading, individuals seeking personal ownership unconnected to militia service have no independent constitutional claim under the Second Amendment; whatever access they retain comes from state statute or other constitutional provisions, not this clause. They are structurally outside the class the reading treats as rights-bearing.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, individual_gun_owners, excluded,
    moderate, biographical, constrained, national).

% Federal agencies and Congress gain latitude to regulate firearms nationally without an individual constitutional right acting as a categorical veto; this reading widens the space of permissible federal and state firearms legislation.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, federal_regulatory_bodies, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__collective_right_reading, federal_regulatory_bodies, agenda_setter).

% Organizations built around defending individual ownership as a constitutional entitlement have no textual foothold under this reading; they must pursue their objectives through legislatures, state constitutions, or litigation grounded in a different reading of the same clause. Their exit is mobile in the sense that they can and do shift strategy to other legal and political fronts.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, gun_rights_advocacy_organizations, excluded,
    organized, generational, mobile, national).

% Historians and legal scholars analyze founding-era militia statutes, ratification debates, and the prefatory/operative clause structure to adjudicate which reading better reflects original public meaning and structural function; they hold no stake in the outcome beyond scholarly credibility.
narrative_ontology:constraint_stakeholder(second_amendment_scope__collective_right_reading, constitutional_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_scope__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves state authority to organize and regulate armed forces (militias) as a check on federal military monopoly, and secondarily preserves broad state and federal latitude to regulate civilian firearms possession without a countervailing individual constitutional claim.
% TRANSFER_FUNCTION: Moves regulatory latitude toward state and federal governments and away from individual claimants; no monetary transfer, but a transfer of legal leverage — governments gain unconstrained rulemaking space, prospective individual claimants lose a constitutional cause of action they would otherwise assert.
% ABSENT_VOICES: Individual gun owners and gun-rights advocacy organizations would object that the reading strips them of a right they believe the text independently guarantees; they are not absent from the broader legal debate but are excluded from the class of rights-holders this specific reading recognizes.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned entirely (courts uniformly adopted an individual-right reading instead), state and federal firearms regulation would face a new constitutional constraint they do not currently face under this reading — enabling stricter judicial scrutiny of gun laws. Governments dispute how much would actually rearrange versus how much is already settled by other doctrines (police power, other enumerated powers); advocacy organizations insist the rearrangement would be substantial.
% FOUNDING_PROBLEM: At Founding, standing federal armies were viewed as threats to liberty; the clause was built to preserve state militia capacity as a decentralized counterweight to federal military power, ensuring states could arm and organize their own defensive forces.
% FOUNDING_PROBLEM_CORROBORATION: Military historians outside the litigation-advocacy ecosystem (on both individual-right and collective-right sides) largely agree that the organized state militia of the Founding era has been superseded by the National Guard system and a professional standing federal military under the Militia Act of 1903 and subsequent statutes — the literal founding problem (states needing an independent check via militia) no longer exists in its original form. Advocacy groups on the individual-right side contest that the underlying purpose (a check on tyranny via an armed populace) survives even if the militia institution does not; that contest is itself evidence the founding-problem status is genuinely disputed rather than settled by any one camp.
narrative_ontology:disappearance_verdict(second_amendment_scope__collective_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__collective_right_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__collective_right_reading_tests).
:- end_tests(second_amendment_scope__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because, by this reading's own lights, the constraint does not reach individual conduct — it is a structural allocation of authority between federal and state government over militia organization, not a mechanism that extracts value or liberty from individuals. Suppression is moderate-low (0.22): the reading does constrain what claims individuals can bring, but it does not itself criminalize or coerce anyone directly — it simply withholds a constitutional cause of action. Resistance is authored high (0.72) because this reading has been the subject of sustained, well-resourced contestation, especially after Heller displaced it doctrinally; genuine coordination readings of contested constitutional text still meet real resistance from parties who prefer a different reading. Accessibility collapse is moderate (0.35) — under this reading, individuals retain other legal avenues (state constitutional provisions, statutory protections) so the collapse is partial, not total.
 *
 * DIRECTIONALITY LOGIC:
 *   State and federal governments are the structural beneficiaries: the reading widens their regulatory latitude and is authored with d near the beneficiary end. Individual gun owners and advocacy organizations are excluded rather than targeted for extraction — this reading does not extract from them, it simply denies them a constitutional lever; they are authored as excluded rather than victims because no cost is transferred TO governments FROM them under this specific clause. This is why base_properties.victims is omitted: the collective-right reading, on its own terms, has no victim class — its structural effect is to leave individual firearms regulation to ordinary majoritarian politics rather than to constitutionalize it either way.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a state-organized militia as a decentralized check on federal military power) is genealogically dead — the National Guard system and professional standing army have superseded the 18th-century militia structure this reading protects. Yet the reading persists as an active doctrinal position with real stakes: if adopted, it removes a constitutional check that would otherwise constrain gun regulation. This is not classic mandatrophy (an extraction dressed as coordination) because the reading claims no ongoing coordination function beyond faithfully describing what the text originally did — its persistence is explained by textual and historical argument, not by rent extraction. The kernel-reading structure lets us hold this coherently: this reading's founding-problem obsolescence does not by itself resolve the sibling readings' claims, because each reading answers the founding-problem question differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_binding_force,
    'Does the prefatory clause (''A well regulated Militia, being necessary to the security of a free State'') legally limit the scope of the operative clause (''the right of the people to keep and bear Arms, shall not be infringed''), or is it merely explanatory and non-binding?',
    'Resolution depends on contested methods of constitutional interpretation (originalist textual analysis of 18th-century grammar and clause structure, ratification-era historical practice, and subsequent judicial precedent) that do not converge on a single answer; this is a live interpretive dispute rather than a fact awaitable by further data.',
    'If the prefatory clause is held binding, this reading''s structural claim holds and state/federal regulatory latitude remains wide. If held non-binding (as Heller found), the individual-right reading displaces this one as controlling doctrine, and this reading survives only as an academic and dissenting position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prefatory_clause_binding_force, conceptual, 'Whether the militia clause limits or merely explains the arms clause — the central textual dispute distinguishing this reading from its siblings.').

omega_variable(
    kernel_reading_authority_shift,
    'Given that Heller (2008) supplanted this reading as controlling Supreme Court doctrine, does this reading''s continued scholarly and dissenting-judicial life constitute a genuinely live position, or is it now primarily a historical/academic reading kept alive by advocacy for reversal?',
    'Track post-Heller lower-court treatment, state constitutional convention debates, and academic citation patterns to determine whether this reading functions as active doctrine anywhere or purely as critique-and-reform advocacy.',
    'If purely academic/advocacy, this reading''s practical extraction and suppression values should be read as historical (pre-2008) rather than current; if it retains live doctrinal force in some state courts or under state constitutions, current-era metrics remain applicable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_authority_shift, empirical, 'Whether this reading is live current doctrine anywhere post-Heller or purely academic/advocacy at present.').

omega_variable(
    collective_right_natural_vs_constructed,
    'Is the state-militia-authority framing a natural reading of federalism-era text and history, or a mid-20th-century interpretive construction responsive to the specific political needs of that era (e.g., New Deal-era deference to federal regulatory authority)?',
    'Comparative analysis of pre-1900 judicial and scholarly treatment of the Second Amendment against 20th-century treatment to see whether the collective-right framing appears organically early or emerges concentrated around specific regulatory episodes (e.g., the National Firearms Act of 1934, United States v. Miller 1939).',
    'If the reading is shown to be a 20th-century regulatory-era construction rather than a continuous historical understanding, its claim to being the more historically faithful reading weakens relative to its rivals.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_right_natural_vs_constructed, conceptual, 'Whether the collective-right framing is a continuous historical reading or a 20th-century regulatory-era interpretive innovation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__collective_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_scope__collective_right_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__collective_right_reading, theater_ratio, 1939, 0.1).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__collective_right_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__collective_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_scope__collective_right_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__collective_right_reading, base_extractiveness, 1791, 0.1).
narrative_ontology:measurement(seco_be_t1900, second_amendment_scope__collective_right_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__collective_right_reading, base_extractiveness, 1939, 0.14).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__collective_right_reading, base_extractiveness, 1980, 0.15).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__collective_right_reading, base_extractiveness, 2008, 0.2).
narrative_ontology:measurement(seco_be_t2025, second_amendment_scope__collective_right_reading, base_extractiveness, 2025, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_scope__collective_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__collective_right_reading, civic_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the second_amendment_scope kernel. The individual_right_reading (which this reading's core axiom forecloses within any single judicial framework, since the two cannot both control the same clause at once) treats the clause as an unconditional personal right; the civic_right_reading is a coexisting intermediate position tying an individual right to civic militia service, which this reading's institutional emphasis puts downstream pressure on without foreclosing it (an individual could still be a state militia member with duties, absent a personal ownership right). All three files share the same kernel_id and text but author different beneficiary sets, different epsilon, and different classifications; none of the three averages over or references the others' metrics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
