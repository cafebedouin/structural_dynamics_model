% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__collective_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__collective_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__collective_right_reading
 *   human_readable: Second Amendment as State-Militia Prerogative (Collective Right Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the collective-right reading of the Second
 *   Amendment kernel: the right runs to state governments' authority to
 *   maintain organized militias, not to individuals apart from militia
 *   service. Under this reading, the amendment functions as a federalism
 *   guarantee against federal disarmament of state militia forces, echoed in
 *   the pre-Heller circuit consensus and grounded doctrinally in United
 *   States v. Miller's militia-nexus requirement. This is a low-extraction
 *   reading precisely because it treats most firearm regulation as ordinary
 *   police power unconstrained by a personal constitutional right — the ε
 *   here reflects the standing arrangement AS THIS READING SEES IT: modest
 *   state institutional friction, no meaningful extraction from individuals
 *   because the reading holds they never had a personal claim to begin with.
 *   The sibling readings (individual_right_reading, civic_republican_reading)
 *   are separate constraint stories with their own ε, beneficiary sets, and
 *   victim structures; this story does not average across them or hedge its ε
 *   to accommodate them.
 *
 * KEY AGENTS:
 *   - state_governments: primary rights-holder and agenda_setter under this reading (institutional/analytical) — retains militia organizing authority
 *   - individual_gun_owners_outside_militia_service: bears the cost of this reading (moderate/constrained) — no personal constitutional claim, subject to plenary regulation
 *   - public_safety_regulators and gun_regulation_advocates: beneficiaries who gain regulatory latitude from the absence of an individual-rights barrier
 *   - originalist_legal_scholars: excluded interpretive community whose textual argument is foreclosed within this reading's own frame
 *   - federal_judiciary: observer seat that historically instantiated this reading (pre-Heller) before displacement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__collective_right_reading, 0.18).
domain_priors:suppression_score(second_amendment_arms_right__collective_right_reading, 0.32).
domain_priors:theater_ratio(second_amendment_arms_right__collective_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(second_amendment_arms_right__collective_right_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__collective_right_reading, rope).
narrative_ontology:human_readable(second_amendment_arms_right__collective_right_reading, "Second Amendment as State-Militia Prerogative (Collective Right Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__collective_right_reading, "constitutional_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__collective_right_reading, '232f94ac-b799-48e8-a1a8-dbc49dcfbe0d').
narrative_ontology:cs_kernel_codification('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', fixed_text).
narrative_ontology:cs_authority_grounding('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', lineage).
narrative_ontology:cs_interpretation_layer_present('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d').
narrative_ontology:cs_reading_relation('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', second_amendment_arms_right__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', second_amendment_arms_right__civic_republican_reading, coexists_with).
narrative_ontology:cs_axiom('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', foundational, prefatory_clause_limits_operative_scope).
narrative_ontology:cs_axiom_status(prefatory_clause_limits_operative_scope, holdable).
narrative_ontology:cs_axiom_grounding('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', prefatory_clause_limits_operative_scope, conventional).
narrative_ontology:cs_axiom('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', foundational, state_militia_as_sole_constitutional_referent).
narrative_ontology:cs_axiom_status(state_militia_as_sole_constitutional_referent, overridden).
narrative_ontology:cs_axiom_grounding('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', state_militia_as_sole_constitutional_referent, conventional).
narrative_ontology:cs_reference_frame('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', miller_militia_nexus_framework).
narrative_ontology:cs_drift_state('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', post_heller_displacement, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('232f94ac-b799-48e8-a1a8-dbc49dcfbe0d', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__collective_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, gun_regulation_advocates).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, public_safety_regulators).
narrative_ontology:constraint_victim(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia_service).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__collective_right_reading, national_guard_members).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, state_militia_primacy_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__collective_right_reading, federalism_over_individual_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, states hold the constitutional prerogative to organize, arm, and discipline militias (now largely instantiated as National Guard units); the amendment is read as a structural federalism guarantee preventing federal disarmament of state militia forces, not as a personal entitlement running to individual residents. States retain broad latitude to regulate private firearm possession as ordinary police power.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, state_governments, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__collective_right_reading, state_governments, beneficiary).

% Legislatures and agencies that draft firearm regulation benefit from this reading because it removes the amendment as an individual-rights barrier to gun control measures — restrictions on possession, sale, and carry outside an organized militia context face only ordinary rational-basis-style scrutiny, not a heightened individual-right standard.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, public_safety_regulators, beneficiary,
    institutional, generational, analytical, national).

% Advocacy organizations pursuing firearm restrictions benefit doctrinally: this reading removes what they view as an anachronistic individual-rights obstacle rooted in a prefatory clause about militias, clearing space for legislative solutions to gun violence without needing to overcome an individual constitutional right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, gun_regulation_advocates, beneficiary,
    organized, generational, mobile, national).

% Private citizens who own or wish to own firearms for self-defense, hunting, or other non-militia purposes bear the cost of this reading: because they hold no personal constitutional claim under this interpretation, their possession is subject to plenary state and federal regulation, including prohibition, up to whatever limit ordinary rational-basis review permits. Their only recourse is the political process, not judicial vindication of an individual right.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, individual_gun_owners_outside_militia_service, payer,
    moderate, biographical, constrained, national).

% As the modern institutional descendant of the organized militia, Guard members are the class this reading treats as the actual rights-bearing context — their unit's access to arms is constitutionally anchored, even though they hold no personal ownership right independent of service.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, national_guard_members, beneficiary,
    organized, generational, constrained, national).

% Scholars and jurists who read the historical record as establishing a pre-existing individual right are not part of this reading's interpretive community; they would object that the prefatory militia clause announces a purpose without limiting the operative clause's individual scope, but this reading treats their historical argument as resolved against them.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, originalist_legal_scholars, excluded,
    organized, generational, analytical, national).

% Courts adjudicate which reading controls in any given era; under this reading's own account, courts that adopted this position (pre-Heller circuit consensus, exemplified by U.S. v. Miller's militia-nexus test) treated individual ownership claims as cognizable only when tied to militia utility.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__collective_right_reading, federal_judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_arms_right__collective_right_reading, state_governments).
narrative_ontology:fixing_cost_class(second_amendment_arms_right__collective_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves state authority to organize, equip, and control militia forces against federal disarmament, and clears the field for legislatures to regulate private firearms as an ordinary exercise of police power without confronting an individual constitutional veto.
% TRANSFER_FUNCTION: Moves the locus of constitutional protection from individual persons to state institutions: state militia (and successor National Guard) structures receive federalism-based insulation, while private individuals outside that organized context receive no personal constitutional shield against firearm regulation, however restrictive.
% ABSENT_VOICES: Individual rights originalists and armed-citizenry theorists are structurally excluded from this reading's interpretive frame; they would argue the operative clause's text ('the right of the people') mirrors identical individual-rights language elsewhere in the Bill of Rights, but this reading treats the prefatory militia clause as controlling and forecloses that argument within its own framework.
% DISAPPEARANCE_RATIONALE: If this reading were entirely displaced (as it substantially was by Heller/McDonald), federal and state firearm regulation would face heightened individual-rights scrutiny, invalidating outright possession bans and reshaping decades of regulatory practice built on the assumption that no personal right constrained legislative discretion — which is in fact what occurred historically when the individual-right reading displaced this one.
% FOUNDING_PROBLEM: Post-Revolutionary anxiety that a standing federal army could supplant or disarm state militias, undermining state sovereignty and the republican check that citizen-soldiers under state control provided against federal military tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the gun-control advocacy community (including scholars who do not share this reading's policy preferences) attest that organized state militias were superseded functionally by the National Guard under the Militia Act of 1903 and federal statutory control, meaning the specific institutional arrangement the clause addressed no longer exists in its founding form; this corroboration comes from military historians and federalism scholars, not primarily from the gun-regulation advocates who benefit from the reading's persistence.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__collective_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__collective_right_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__collective_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__collective_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__collective_right_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__collective_right_reading_tests).
:- end_tests(second_amendment_arms_right__collective_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because, under this reading's own lights, the constraint does not extract from individuals at all in a constitutional sense — it simply declines to recognize a personal right, leaving ordinary majoritarian politics to set firearm policy. The small extraction that exists reflects a mild transfer from individual autonomy expectations to state regulatory discretion. Suppression (0.32) is moderate: this reading does not require active coercive enforcement of a substantive rule so much as judicial deference to legislative firearm regulation — resistance (0.75) is high because it is a heavily contested reading (later doctrinally displaced by Heller/McDonald), so its persistence required sustained argument against a competing textualist reading, not physical suppression of alternatives. Accessibility collapse (0.4) is moderate-low: individual-right arguments were never eliminated from legal discourse, only judicially disfavored for a period.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and firearms regulators are structural beneficiaries (d near beneficiary end): the reading expands their regulatory latitude and insulates militia-organizing authority from federal preemption. Individual gun owners outside militia service are the structural target (d near full-target end): under this reading they hold no personal constitutional shield, so any restrictive regulation reaches them without a heightened-scrutiny check. National Guard members occupy an intermediate position — they benefit institutionally through their unit's constitutional anchoring but hold no greater personal ownership right than any other civilian.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (fear of federal disarmament of state militias as a check on standing-army tyranny) is functionally dead: the Militia Act of 1903 and subsequent federalization of the National Guard resolved the original institutional anxiety by placing state militia forces under substantial federal statutory integration. This reading's continued advocacy after that resolution risks exactly the founding_problem_status=dead + disappearance_verdict=world_rearranges mismatch pattern — the reading persists as a live doctrinal position not because the militia-disarmament problem remains active, but because it now serves the freestanding function of preserving legislative latitude over an unrelated, later-arising policy question (urban gun violence, mass shootings) that the original constitutional bargain never contemplated. This is not dispositive against the reading's validity as constitutional interpretation, but it is the genealogical fact an honest six-questions interview must surface.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prefatory_clause_operative_clause_relationship,
    'Does the prefatory militia clause (''A well regulated Militia...'') limit the scope of the operative clause (''the right of the people to keep and bear Arms''), or does it merely announce a purpose without narrowing an independently individual right?',
    'This is the central textual and historical dispute between the collective_right_reading and individual_right_reading; it is resolved (differently, in each era) by the composition of the adjudicating court and the historical linguistics evidence it credits — it is not resolvable by new empirical data but by which interpretive methodology and historical corpus a court treats as authoritative.',
    'If the prefatory clause is read as limiting (this reading''s premise), regulation faces only ordinary scrutiny; if read as non-limiting (the individual_right_reading''s premise), regulation faces heightened individual-rights scrutiny. The two readings cannot both control simultaneously within a single court''s doctrine, though they can and do coexist as live positions across different courts, eras, and scholarly traditions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(prefatory_clause_operative_clause_relationship, conceptual, 'Whether the militia clause narrows or merely explains the operative right — the kernel''s central interpretive fork.').

omega_variable(
    militia_institutional_continuity,
    'Is the National Guard the true institutional successor to the founding-era organized militia such that this reading''s protection meaningfully transfers to it, or did federalization (1903 Militia Act onward) so transform the institution that the original constitutional bargain no longer has a live referent?',
    'Historical and administrative-law analysis of the degree of federal control over Guard units (dual-status command, federalization triggers, funding structure) compared to founding-era state militia autonomy.',
    'If institutional continuity is thin, the founding_problem_status=dead determination is strengthened and this reading''s contemporary persistence looks more like doctrinal inertia than genealogical fidelity; if continuity is judged substantial, the reading''s contemporary application is better grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_institutional_continuity, empirical, 'Whether the National Guard is a genuine institutional continuation of the constitutional militia or a functionally distinct federal creation.').

omega_variable(
    kernel_framing_underdetermination,
    'Should this reading be evaluated primarily against the founding-era historical record (an originalist framing, which is contested territory this reading also claims) or against the doctrinal trajectory of 20th-century case law (a living-constitutionalism / stare-decisis framing, where this reading held sway for decades via Miller)?',
    'The two framings would classify this reading''s naturalness and persistence differently: the originalist framing treats the 2008 Heller displacement as a correction of long-standing interpretive error, while the stare-decisis framing treats it as a doctrinal reversal that unsettled genuine reliance interests built over 70 years of Miller-based regulation.',
    'Under the originalist framing this reading might be judged a poorly grounded historical claim now correctly abandoned; under the stare-decisis framing it might be judged a stable, long-settled coordination equilibrium disrupted by a later court. This story adopts the reading''s own self-understanding (a genealogically grounded federalism claim) as its authoring frame, consistent with treating ε as authored from the reading''s own lights per the kernel-reading referent rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Alternative framings (originalist vs. stare-decisis) would classify this reading''s stability and legitimacy differently.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__collective_right_reading, 1791, 2008).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__collective_right_reading, theater_ratio, 1791, 0.05).
narrative_ontology:measurement(seco_tr_t1850, second_amendment_arms_right__collective_right_reading, theater_ratio, 1850, 0.08).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__collective_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__collective_right_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement(seco_tr_t1970, second_amendment_arms_right__collective_right_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(seco_tr_t1990, second_amendment_arms_right__collective_right_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__collective_right_reading, theater_ratio, 2008, 0.22).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1791, 0.08).
narrative_ontology:measurement(seco_be_t1850, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1850, 0.1).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1900, 0.12).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1939, 0.14).
narrative_ontology:measurement(seco_be_t1970, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1970, 0.16).
narrative_ontology:measurement(seco_be_t1990, second_amendment_arms_right__collective_right_reading, base_extractiveness, 1990, 0.17).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__collective_right_reading, base_extractiveness, 2008, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_arms_right__collective_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__collective_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_arms_right__collective_right_reading, 0.1).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__collective_right_reading, civic_republican_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the second_amendment_arms_right kernel. The individual_right_reading (currently the controlling doctrine post-Heller) authors a markedly different beneficiary/victim structure (individual owners as beneficiaries, regulators as constrained) and a higher ε on prohibition measures, since it treats outright bans as extraction from a recognized personal right. The civic_republican_reading occupies an intermediate structural position, treating armed citizenship as instrumentally tied to self-governance rather than either pure state prerogative or pure individual entitlement. All three share the same fixed constitutional text and founding-era record as their common kernel but diverge sharply in the coordination function, beneficiary/victim assignment, and extraction the arrangement is read to produce.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
