% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: The Nuclear Taboo — Constructed Normative Prohibition on Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This story instantiates the nuclear-taboo reading of the
 *   total_war_possibility_space kernel: total war remains materially
 *   possible, but a constructed normative prohibition — independent of
 *   underlying capability and deterrence math — has foreclosed it as a live
 *   policy option. The taboo is treated here as an intersubjective fact
 *   requiring continuous discursive and institutional maintenance
 *   (non-proliferation regime, no-first-use pledges, moral stigma) rather
 *   than as a mere restatement of deterrence equilibrium or a claim about the
 *   collapse of strategic space itself. Those alternative claims are separate
 *   constraints in this kernel (deterrence_equilibrium_reading,
 *   space_contraction_reading) and are not blended into this ε.
 *
 * KEY AGENTS:
 *   - nuclear_weapon_states: agenda_setter/beneficiary (institutional/arbitrage) — write and selectively invoke the taboo while retaining capability
 *   - norm_entrepreneur_ngos: beneficiary (organized/mobile) — institutional survival tied to taboo salience
 *   - nonproliferation_treaty_secretariat: agenda_setter/beneficiary (institutional/constrained) — administers enforcement apparatus
 *   - non_nuclear_weapon_states: payer (moderate/constrained) — bound by prohibition without possessing the traded-off capability
 *   - threshold_states_denied_deterrent: payer (moderate/trapped) — face sanctions for crossing a threshold defined by others
 *   - populations_under_extended_deterrence_umbrella: payer/beneficiary (powerless/trapped) — live inside the ambiguity the taboo creates
 *   - military_planning_establishments: excluded (organized/constrained) — treat total war as live contingency regardless of taboo rhetoric
 *   - strategic_studies_scholars: observer (analytical) — contest whether the taboo does independent causal work
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.42).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.55).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "The Nuclear Taboo — Constructed Normative Prohibition on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, 'd21cacc7-beba-4e84-9bd2-beb630b0513a').
narrative_ontology:cs_kernel_codification('d21cacc7-beba-4e84-9bd2-beb630b0513a', distributed).
narrative_ontology:cs_authority_grounding('d21cacc7-beba-4e84-9bd2-beb630b0513a', distributed).
narrative_ontology:cs_reading_relation('d21cacc7-beba-4e84-9bd2-beb630b0513a', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('d21cacc7-beba-4e84-9bd2-beb630b0513a', total_war_possibility_space__space_contraction_reading, influences).
narrative_ontology:cs_axiom('d21cacc7-beba-4e84-9bd2-beb630b0513a', foundational, prohibition_is_normatively_constructed_not_materially_derived).
narrative_ontology:cs_axiom_status(prohibition_is_normatively_constructed_not_materially_derived, holdable).
narrative_ontology:cs_axiom_grounding('d21cacc7-beba-4e84-9bd2-beb630b0513a', prohibition_is_normatively_constructed_not_materially_derived, conventional).
narrative_ontology:cs_axiom('d21cacc7-beba-4e84-9bd2-beb630b0513a', foundational, taboo_strength_independent_of_underlying_capability).
narrative_ontology:cs_axiom_status(taboo_strength_independent_of_underlying_capability, holdable).
narrative_ontology:cs_axiom_grounding('d21cacc7-beba-4e84-9bd2-beb630b0513a', taboo_strength_independent_of_underlying_capability, empirically_contingent).
narrative_ontology:cs_reference_frame('d21cacc7-beba-4e84-9bd2-beb630b0513a', post_hiroshima_category_formation).
narrative_ontology:cs_drift_state('d21cacc7-beba-4e84-9bd2-beb630b0513a', post_cold_war_proliferation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d21cacc7-beba-4e84-9bd2-beb630b0513a', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_ngos).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nonproliferation_treaty_secretariat).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, arms_control_epistemic_community).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, threshold_states_denied_deterrent).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, populations_under_extended_deterrence_umbrella).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, populations_under_extended_deterrence_umbrella).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, constructivist_norm_life_cycle_theory).
narrative_ontology:constraint_vindicates(total_war_possibility_space__nuclear_taboo_reading, taboo_as_independent_causal_variable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the material capability for total war but publicly bind themselves to a normative prohibition (no-first-use pledges, taboo rhetoric) that they administer, interpret, and can selectively invoke or waive. Retain arsenals while claiming the constraint on use is normative rather than capability-based; occupy the seat that both writes the taboo's content and benefits from the legitimacy it confers on their continued possession.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, beneficiary).

% Anti-nuclear advocacy networks, disarmament campaigners, and epistemic communities whose institutional purpose and funding depend on the taboo's continued salience as a live normative fact. They produce the discourse that constitutes the taboo — testimony, treaties, moral framing — and their organizational survival benefits from the taboo needing continuous defense.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneur_ngos, beneficiary,
    organized, generational, mobile, global).

% Administers the verification and enforcement apparatus (IAEA safeguards, review conferences) that operationalizes the taboo as policy. Its institutional existence is coextensive with the taboo's enforcement; it has no independent function if the taboo is understood as mere deterrence math rather than normative prohibition.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nonproliferation_treaty_secretariat, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, nonproliferation_treaty_secretariat, beneficiary).

% Bound by the same normative prohibition as nuclear states but without the underlying capability that would let them treat the taboo as elective. They forgo development, submit to inspection regimes, and absorb the taboo's constraint as binding law rather than as a strategic choice they could reverse at will — the taboo's suppressive force falls on them without the offsetting deterrent good it gives possessor states.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, national).

% States with latent capability (enrichment infrastructure, delivery systems) who face sanctions, isolation, and moral condemnation if they cross the threshold the taboo defines. The taboo's content is written by states that already possess the weapon; threshold states experience it as a closing door rather than a norm they helped author.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, threshold_states_denied_deterrent, payer,
    moderate, biographical, trapped, regional).

% Civilian populations of allied non-nuclear states whose security depends on the credibility of a patron's willingness to break the taboo on their behalf if pushed. They benefit from the umbrella's deterrent effect while bearing the risk that the taboo's normative force is precisely what makes the umbrella's promise incredible — the constraint they live under is the ambiguity itself.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, populations_under_extended_deterrence_umbrella, payer,
    powerless, civilizational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(total_war_possibility_space__nuclear_taboo_reading, populations_under_extended_deterrence_umbrella, beneficiary).

% Professional military planners who model total-war scenarios as contingencies regardless of the taboo's normative status. Their operational planning treats the taboo as a political constraint layered atop, not replacing, material calculation — a view largely excluded from the diplomatic and academic discourse that constitutes the taboo's public content.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, military_planning_establishments, excluded,
    organized, biographical, constrained, national).

% Academic analysts (constructivist and realist alike) who debate whether the taboo is doing independent causal work or is epiphenomenal to deterrence and capability constraints. Their disagreement is definitional to this reading's contested status within the broader kernel.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, strategic_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The taboo coordinates mutual restraint among states that could otherwise treat nuclear use as a live tactical option, by constituting a shared normative category ('unthinkable,' 'beyond the pale') that lowers the probability any single actor crosses the threshold first, independent of whatever deterrence math also applies.
% TRANSFER_FUNCTION: Moves legitimacy and freedom of action from non-possessor and threshold states toward possessor states: the taboo is written and administered by those who already hold the weapon, who thereby retain arsenals under a norm that simultaneously stigmatizes acquisition by everyone else.
% ABSENT_VOICES: Military planning establishments who treat total war as a live contingency regardless of taboo rhetoric are structurally excluded from the diplomatic and constructivist discourse that constitutes the taboo's public content; threshold states denied the deterrent had no seat in drafting the norm that now binds them.
% DISAPPEARANCE_RATIONALE: Deterrence-equilibrium readers hold the world would barely change if the taboo vanished overnight, since material deterrence would still restrain use; taboo readers hold that removing the constructed normative prohibition would measurably raise the probability of first use in a crisis, because the taboo does causal work beyond what capability and retaliation math alone would produce. The disagreement is the kernel contest itself.
% FOUNDING_PROBLEM: After Hiroshima and Nagasaki, and accelerating through the Cold War, policymakers and publics needed a way to make nuclear use something other than an ordinary escalatory option on a continuum with conventional force — a category shift from 'very large weapon' to 'category apart,' constructed through repeated non-use, moral condemnation, and institutionalized restraint norms.
% FOUNDING_PROBLEM_CORROBORATION: Norm entrepreneurs and much of the constructivist IR literature (Tannenwald and successors) attest the taboo is live and doing independent causal work, evidenced by 80 years of non-use despite capability. Realist and deterrence-theory scholars, writing from outside the norm-entrepreneur community, attest the non-use record is equally consistent with rational deterrence and offers no clean test distinguishing the two; some military planners attest privately that operational contingency planning proceeds as if the taboo were a political constraint layered on top of, not a replacement for, capability-based restraint.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, contested).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42 at interval end) because the taboo's operation redistributes legitimacy and strategic latitude toward possessor states without a clean monetary or resource transfer — the extraction is normative and positional. Suppression is authored higher (0.55) because maintaining the taboo requires active discursive and institutional work: stigmatization campaigns, treaty enforcement, sanctions regimes against threshold crossers. Theater ratio rises over the interval (0.10 to 0.38) reflecting the accumulation of performative non-proliferation diplomacy (review conferences, disarmament pledges by possessor states that do not disarm) alongside the taboo's genuine restraint function. Accessibility collapse (0.62) is moderate-high: once the taboo is institutionalized, exit from the discourse (treating nuclear use as an ordinary escalatory option) becomes reputationally almost unthinkable for policymakers, though not physically impossible — this is precisely why the taboo reading is distinct from the space_contraction reading, which claims the option collapsed from the strategically thinkable altogether rather than merely the normatively sayable.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon states' seat, the taboo is a voluntarily assumed restraint that enhances everyone's security and costs them freedom of action they are choosing to forgo. From the non-nuclear and threshold states' seat, the same structure is an asymmetric prohibition authored by incumbents that forecloses their own acquisition options while incumbents retain theirs — coordination and extraction riding the same normative machinery, which is exactly the tangled_rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states sit near the beneficiary end: they administer the taboo's content, retain the underlying capability, and gain legitimacy for restraint they were never going to exercise anyway under most crisis scenarios (deterrence would have restrained them regardless — the taboo reading's distinct causal claim is that it restrains them further). Non-nuclear and threshold states sit near the target end: they bear the taboo's suppressive force (inspections, sanctions, stigma) without holding the capability that would make the norm elective for them. Norm entrepreneurs and the treaty secretariat are structural beneficiaries whose institutional existence depends on continued taboo salience — this is a genuine coordination benefit (they solve a real collective-action problem in reducing use probability) bundled with an extraction dynamic (their funding and mandate depend on the problem never being declared solved).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (making nuclear use categorically unthinkable rather than merely tactically deterred) remains genuinely contested rather than resolved-and-abandoned: it is not a dead mandate persisting by inertia (which would point to piton), because both live restraint function and live extraction are still operating simultaneously, and enforcement is actively maintained rather than vestigial. The disappearance_verdict is authored as contested rather than world_rearranges specifically because whether the taboo is doing independent causal work, versus being epiphenomenal to deterrence, is the substance of the kernel dispute — this story does not resolve that dispute, it represents one party's position within it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_vs_deterrence_independent_variable,
    'Is the nuclear taboo doing independent causal work in preventing use, or is 80 years of non-use fully explained by rational deterrence (mutual vulnerability) without any need for a normative-construction variable?',
    'Comparative case analysis of crisis decision-making transcripts (Cuban Missile Crisis, Able Archer, Kargil) for explicit invocation of moral/taboo language versus pure cost-benefit deterrence calculus in the actual reasoning of decision-makers, corroborated by declassified archives rather than post-hoc justification.',
    'If deterrence alone explains the record, this reading collapses into the deterrence_equilibrium_reading and the taboo apparatus is better classified as theater riding on a materially sufficient mechanism (pushing this constraint toward piton). If taboo language demonstrably altered decisions at the margin independent of deterrence calculus, the tangled_rope classification with genuine coordination function is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_independent_variable, empirical, 'Whether the taboo is a causally independent variable or epiphenomenal to deterrence.').

omega_variable(
    taboo_asymmetry_natural_or_constructed,
    'Is the taboo''s asymmetric application (binding on non-possessors more than possessors) an inherent feature of any workable non-proliferation norm, or is it a constructed extraction that could in principle be restructured toward symmetric disarmament obligations?',
    'Examine NPT Article VI disarmament-obligation enforcement history: if possessor-state disarmament commitments are pursued with comparable rigor to non-possessor verification, the asymmetry is transitional rather than structural.',
    'Persistent asymmetry with no disarmament enforcement trajectory supports the tangled_rope reading (genuine coordination function bundled with entrenched asymmetric extraction); movement toward symmetric obligations would support reclassifying toward scaffold (transitional coordination with an eventual sunset).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_asymmetry_natural_or_constructed, conceptual, 'Whether the taboo''s beneficiary asymmetry is structural or a removable transitional feature.').

omega_variable(
    norm_entrepreneur_exit_fragility,
    'How dependent is the taboo''s continued force on the active presence of norm-entrepreneur institutions (NGOs, epistemic communities, treaty bodies) — would it persist if those institutions were defunded or delegitimized?',
    'Track taboo salience (public discourse framing, elite rhetoric) in states where anti-nuclear NGO presence and funding has declined, controlling for other variables, to test whether taboo strength tracks institutional maintenance effort.',
    'If taboo strength tracks norm-entrepreneur institutional health rather than persisting independently, this corroborates the reading''s own structural-delta prediction (taboo weakens if norm entrepreneurs exit) and strengthens the tangled_rope classification''s active-enforcement requirement; if taboo strength is robust to entrepreneur decline, the norm may have become internalized past the point of needing active maintenance (approaching mountain-like naturalization, a false-summit risk worth flagging separately).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_fragility, empirical, 'Whether taboo persistence depends on active norm-entrepreneur maintenance, per this reading''s own predicted structural delta.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1962, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1962, 0.18).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(tota_tr_t1995, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(tota_tr_t2010, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(tota_tr_t2025, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1962, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1962, 0.25).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.33).
narrative_ontology:measurement(tota_be_t1995, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(tota_be_t2010, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(tota_be_t2025, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(tota_su_t1962, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1962, 0.35).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(tota_su_t1995, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(tota_su_t2010, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2010, 0.52).
narrative_ontology:measurement(tota_su_t2025, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(total_war_possibility_space__nuclear_taboo_reading, 0.1).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, deterrence_equilibrium_reading).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, space_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the total_war_possibility_space kernel, each instantiated as a separate constraint story per the ε-invariance principle. deterrence_equilibrium_reading claims total war is strategically reachable but materially deterred (a mountain-adjacent, capability-grounded account with negligible normative-construction component). space_contraction_reading claims the strategic option space itself collapsed, not merely the normative evaluation of options within it (the strongest, most mountain-like claim of the three). This story (nuclear_taboo_reading) claims an intermediate position: material possibility persists, but normative prohibition — constructed, contested, and requiring active maintenance — does independent causal work, which is why this reading alone generates the tangled_rope classification among the three siblings and carries the non-proliferation enforcement apparatus as its distinctive structural signature.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
