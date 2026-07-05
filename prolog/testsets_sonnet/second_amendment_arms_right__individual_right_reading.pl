% ============================================================================
% CONSTRAINT STORY: second_amendment_arms_right__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_arms_right__individual_right_reading, []).

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
 *   constraint_id: second_amendment_arms_right__individual_right_reading
 *   human_readable: Second Amendment as Pre-Existing Individual Right (Heller/McDonald Reading)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the individual-right reading of the Second
 *   Amendment's kernel text: the claim that the right to keep and bear arms
 *   belongs to individuals as a pre-political liberty, enforceable against
 *   government infringement regardless of militia affiliation. This reading
 *   became doctrinally dominant with District of Columbia v. Heller (2008)
 *   and was incorporated against the states in McDonald v. City of Chicago
 *   (2010), then substantially expanded in scope-of-scrutiny terms by New
 *   York State Rifle & Pistol Association v. Bruen (2022). Before Heller, the
 *   individual-right reading existed primarily as a minority scholarly and
 *   advocacy position; its extraction profile at the founding and through
 *   most of the 20th century was low because it had little doctrinal force.
 *   Its extraction profile rises sharply after 2008 as the reading becomes
 *   the operative constitutional rule displacing legislative and regulatory
 *   judgment across jurisdictions.
 *
 * KEY AGENTS:
 *   - individual_gun_owners: Primary beneficiary (organized/mobile) — holds the entrenched liberty
 *   - firearms_industry: Secondary beneficiary (powerful/arbitrage) — benefits from a deregulated legal floor
 *   - gun_rights_advocacy_organizations: Agenda-setter (organized/arbitrage) — administers doctrinal expansion via litigation
 *   - municipal_regulators: Primary payer (institutional/constrained) — loses policymaking authority to constitutional scrutiny
 *   - gun_violence_survivors_and_families: Diffuse payer (powerless/trapped) — bears downstream harm from invalidated regulation
 *   - constitutional_historians: Analytical observer — assesses the historical record's actual support for the reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_arms_right__individual_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_arms_right__individual_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_arms_right__individual_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(second_amendment_arms_right__individual_right_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_arms_right__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_arms_right__individual_right_reading, "Second Amendment as Pre-Existing Individual Right (Heller/McDonald Reading)").
narrative_ontology:topic_domain(second_amendment_arms_right__individual_right_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_arms_right__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_arms_right__individual_right_reading, 'bfe4a973-d4c4-41fd-b478-73738af2d1fb').
narrative_ontology:cs_kernel_codification('bfe4a973-d4c4-41fd-b478-73738af2d1fb', fixed_text).
narrative_ontology:cs_authority_grounding('bfe4a973-d4c4-41fd-b478-73738af2d1fb', lineage).
narrative_ontology:cs_interpretation_layer_present('bfe4a973-d4c4-41fd-b478-73738af2d1fb').
narrative_ontology:cs_reading_relation('bfe4a973-d4c4-41fd-b478-73738af2d1fb', second_amendment_arms_right__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('bfe4a973-d4c4-41fd-b478-73738af2d1fb', second_amendment_arms_right__civic_republican_reading, influences).
narrative_ontology:cs_axiom('bfe4a973-d4c4-41fd-b478-73738af2d1fb', foundational, arms_right_predates_and_survives_government).
narrative_ontology:cs_axiom_status(arms_right_predates_and_survives_government, holdable).
narrative_ontology:cs_axiom_grounding('bfe4a973-d4c4-41fd-b478-73738af2d1fb', arms_right_predates_and_survives_government, deontological).
narrative_ontology:cs_axiom('bfe4a973-d4c4-41fd-b478-73738af2d1fb', secondary, militia_clause_is_prefatory_not_operative).
narrative_ontology:cs_axiom_status(militia_clause_is_prefatory_not_operative, holdable).
narrative_ontology:cs_axiom_grounding('bfe4a973-d4c4-41fd-b478-73738af2d1fb', militia_clause_is_prefatory_not_operative, conventional).
narrative_ontology:cs_reference_frame('bfe4a973-d4c4-41fd-b478-73738af2d1fb', founding_era_natural_rights_liberalism).
narrative_ontology:cs_drift_state('bfe4a973-d4c4-41fd-b478-73738af2d1fb', post_heller_doctrinal_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('bfe4a973-d4c4-41fd-b478-73738af2d1fb', '').
narrative_ontology:cs_kernel_id(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, firearms_industry).
narrative_ontology:constraint_beneficiary(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, gun_violence_survivors_and_families).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, municipal_regulators).
narrative_ontology:constraint_victim(second_amendment_arms_right__individual_right_reading, domestic_violence_victims_in_permissive_jurisdictions).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, natural_rights_pre_political_theory).
narrative_ontology:constraint_vindicates(second_amendment_arms_right__individual_right_reading, originalist_constitutional_methodology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold a constitutionally entrenched right to keep and bear arms for self-defense, independent of militia service or state permission. This reading converts firearm ownership from a state-granted privilege into a shielded liberty; they can challenge restrictive local ordinances in federal court and have done so successfully post-Heller.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, individual_gun_owners, beneficiary,
    organized, biographical, mobile, national).

% Manufactures and sells firearms into a market whose legal floor is set by the individual-right reading; the constitutional guarantee forecloses many demand-suppressing regulations before they reach legislatures, and industry-aligned groups fund litigation extending the doctrine into new domains (assault weapons, magazine capacity, carry permitting).
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, firearms_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Litigate test cases, draft model legislation, and fund the originalist scholarship that undergirds the individual-right doctrine. They administer the doctrine's expansion by selecting which restrictions to challenge and which historical analogues to press before courts, converting a constitutional holding into an ongoing deregulatory program.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, agenda_setter,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_arms_right__individual_right_reading, gun_rights_advocacy_organizations, beneficiary).

% City and state governments that once set firearm policy through ordinary legislation now must survive strict constitutional scrutiny (Bruen's text-history-tradition test) for any regulation. Waiting periods, permit-to-purchase schemes, and carry restrictions enacted for public safety are struck down unless a sufficiently close historical analogue existed at the founding or Reconstruction, regardless of contemporary evidentiary support for the regulation's effectiveness.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, municipal_regulators, payer,
    institutional, biographical, constrained, regional).

% Bear the downstream cost when regulations that would have reduced firearm access to high-risk individuals are invalidated under the individual-right framework. They have no seat in the constitutional litigation that sets the floor of permissible regulation and cannot exit the jurisdiction's resulting risk profile without relocating.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, gun_violence_survivors_and_families, payer,
    powerless, biographical, trapped, national).

% Live in jurisdictions where firearm-removal statutes for restraining-order respondents face individual-right challenges; the doctrine's expansion into as-applied challenges (contested even post-Rahimi) creates gaps in protective enforcement that fall disproportionately on this group, who typically cannot relocate on short notice.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, domestic_violence_victims_in_permissive_jurisdictions, payer,
    powerless, immediate, trapped, regional).

% Elected bodies that would otherwise set firearm policy through ordinary democratic processes and empirical safety review are displaced from that role once the individual-right doctrine constitutionalizes the floor; legislative judgment about contemporary risk is subordinated to a historical-analogue test that does not admit modern public-health evidence as dispositive.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, federal_and_state_legislatures, excluded,
    institutional, generational, constrained, national).

% Assess whether the founding-era historical record actually supports an individual, non-militia-contingent right, and whether the doctrine's application via analogical reasoning is methodologically sound or a results-driven veneer over policy preference.
narrative_ontology:constraint_stakeholder(second_amendment_arms_right__individual_right_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, judicially enforceable baseline that individual citizens can rely on when planning self-defense arrangements, insulating a core liberty from shifting legislative majorities and giving firearms commerce and ownership planning long-term legal certainty.
% TRANSFER_FUNCTION: Moves the burden of proof and the practical costs of firearm-related harm from firearm owners and industry (who face fewer regulatory constraints) to potential victims of firearm violence and to regulatory bodies (who must litigate defensively and often lose), without a corresponding transfer of resources to mitigate the resulting harms.
% ABSENT_VOICES: Gun violence survivors, domestic violence victims, and public health researchers are not parties to the constitutional litigation that sets the doctrine's scope; their empirical evidence on regulation effectiveness is systematically excluded by a test that asks only whether a historical analogue existed, not whether the regulation works.
% DISAPPEARANCE_RATIONALE: If the individual-right reading were overnight replaced by the collective-right reading, hundreds of state and local firearm regulations currently vulnerable to constitutional challenge would become durable again, litigation funded by gun-rights organizations would lose its primary vehicle, and legislatures would regain primary policymaking authority over firearms — a substantial reallocation of practical power between regulators and owners.
% FOUNDING_PROBLEM: The reading was advanced to resolve genuine ambiguity in an 18th-century text whose prefatory militia clause and operative individual-right clause admit more than one coherent parsing, and to check what its proponents viewed as creeping disarmament of law-abiding citizens through incremental local ordinances.
% FOUNDING_PROBLEM_CORROBORATION: Individual-right proponents and originalist scholars attest the ambiguity is real and their reading resolves it correctly. Legal historians outside the gun-rights advocacy network (including some originalist scholars who reach the collective-right or civic-republican conclusion from the same historical record) dispute both the historical premise and the doctrine's current application via analogical reasoning in Bruen; this dispute is documented in amicus briefs and academic literature independent of either advocacy side.
narrative_ontology:disappearance_verdict(second_amendment_arms_right__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_arms_right__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_arms_right__individual_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_arms_right__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_arms_right__individual_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_arms_right__individual_right_reading_tests).
:- end_tests(second_amendment_arms_right__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.42 — moderate rather than severe — because the coordination function (a stable, judicially enforceable individual liberty) is genuine and not merely cover: many core applications (self-defense in the home, ownership by law-abiding citizens) impose no identifiable victim. The extraction concentrates specifically in the doctrine's use to invalidate empirically-supported public-safety regulation via the Bruen historical-analogue test, which is where the victim set (survivors, domestic violence victims, regulators) is concentrated. Suppression (0.38) reflects the active judicial enforcement required to strike down contrary state and local law — this is not a passive liberty but one requiring continuous litigation to maintain its scope. Theater ratio is comparatively low (0.22): the doctrine does substantive legal work (actual invalidation of actual statutes), it is not merely symbolic. Resistance is high (0.71) because municipal regulators, public health advocates, and gun-safety organizations actively contest the doctrine's scope in nearly every jurisdiction, and academic historians actively contest its founding-era premises. Accessibility collapse is moderate (0.48): regulatory alternatives (permit schemes, waiting periods, safe-storage laws) are not eliminated outright but must survive an increasingly narrow historical-analogue test, so many alternatives collapse post-Bruen while others survive.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual gun owners and the firearms industry sit near the beneficiary end: the constitutional floor subsidizes their position by removing regulatory risk they would otherwise face through ordinary legislative processes. Gun rights advocacy organizations are agenda-setters who administer the doctrine's expansion — they collect litigation influence and donor support without bearing the downstream costs. Municipal regulators are structurally constrained: they retain formal authority to legislate but face a high probability of judicial invalidation, which functions as an effective override of their institutional power. Gun violence survivors and domestic violence victims in permissive jurisdictions are the clearest targets: trapped exit options (they cannot simply relocate away from a state's firearm regulatory environment on short notice), full-target directionality, and no voice in the litigation that sets the doctrine's scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — resolving genuine textual ambiguity about whether the right is militia-contingent or individual — was live in the sense that reasonable readings diverge and the ambiguity is textually real, not manufactured. What prevents this from being simple mandatrophy is that the coordination function (protecting an individual liberty from majoritarian erosion) remains genuinely contested rather than obviously obsolete; unlike a scaffold whose transitional purpose has plainly expired, the individual-right reading's proponents can point to an ongoing, non-frivolous dispute about disarmament risk. The tangled_rope classification (rather than snare) reflects that the coordination function is real for a core category of beneficiaries even as the doctrine is actively used, at its margins, to override public-safety judgments with disproportionate and diffuse cost to non-parties. Labeling this a pure mountain (as some individual-right proponents frame it — a pre-existing natural right merely 'recognized') would erase the doctrine's active enforcement machinery and its distributional consequences; labeling it a pure snare would erase the genuine liberty interest at its core. It requires the hybrid category.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    second_amendment_reading_kernel_individual_right,
    'This constraint instantiates the individual-right reading of the second_amendment_arms_right kernel — the claim that the right is a pre-political individual liberty unconnected to militia service. The sibling readings (collective_right_reading: the right protects state militia authority, not individual ownership; civic_republican_reading: the right protects armed citizenship as prerequisite for republican self-governance, neither purely individual nor state-centered) would each produce a structurally different constraint with a different beneficiary/victim set and a different ε. Which reading correctly describes the founding-era text and practice?',
    'Resolution would require either a definitive historical consensus on 18th-century usage of ''the people,'' ''keep and bear arms,'' and militia clauses (which the historical record does not currently supply — historians remain divided), or a settled interpretive-methodology consensus (originalism vs. living constitutionalism vs. structural inference) that the legal community does not currently share. Absent either, the disagreement is irreducible at the level of this framework.',
    'Under collective_right_reading, individual gun owners exit the beneficiary set and state/federal regulatory authority is no longer a constrained party, producing a low-ε, low-suppression rope or mountain-adjacent classification. Under civic_republican_reading, the beneficiary set includes armed citizens qua participants in self-governance rather than qua individual self-defenders, changing the coordination function''s description and likely lowering measured extraction relative to this reading. This reading (individual_right_reading) produces the highest ε of the three because it is the reading whose doctrinal application most directly displaces contemporary legislative and regulatory judgment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(second_amendment_reading_kernel_individual_right, conceptual, 'Kernel-reading indeterminacy: which reading of the Second Amendment kernel is historically and structurally correct is not resolved by this story and is routed here rather than hedged into ε.').

omega_variable(
    natural_rights_versus_constructed_doctrine,
    'Is the individual right to keep and bear arms genuinely pre-political (a natural liberty the Constitution merely recognizes, as Heller''s own language claims) or is it a constructed doctrinal artifact of late-20th-century originalist legal mobilization that reads a pre-existing right backward into an ambiguous 18th-century text?',
    'Historical linguistics and comparative constitutional analysis of contemporaneous state constitutions and English common-law antecedents could partially resolve the ''pre-existing'' claim; the doctrinal-mobilization question is more directly evidenced by the documented history of the individual-right reading''s rise from fringe position (pre-1970s) to mainstream conservative legal movement position to Supreme Court doctrine (Heller, 2008), which is well-documented in legal history scholarship.',
    'If genuinely pre-political and merely recognized, the mountain framing gains force and the beneficiary declarations describe incidental beneficiaries of a natural fact rather than of a constructed rule — though FSM logic would still apply given identifiable beneficiaries. If substantially constructed through mobilization, the tangled_rope classification is reinforced: coordination function is real but the doctrine''s specific current scope reflects successful interest-group legal strategy more than timeless liberty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_rights_versus_constructed_doctrine, conceptual, 'Whether the individual right is discovered natural law or constructed doctrine — bears on false-summit-style framing even within this non-mountain reading.').

omega_variable(
    historical_analogue_test_methodology,
    'Is the Bruen ''text, history, and tradition'' analogical-reasoning test a principled originalist methodology, or is it a results-oriented mechanism that can be tuned (via selection of which historical analogues count as ''relevantly similar'') to reach predetermined deregulatory outcomes?',
    'Track lower-court applications of the Bruen test across circuits for consistency; if courts applying the same methodology to similar regulations reach highly divergent outcomes, that inconsistency is evidence the test underdetermines results and is being filled in by policy preference.',
    'If the test is principled and consistently applied, the suppression and extraction figures reflect a stable, predictable doctrine. If the test is manipulable, the effective extraction is understated by current metrics because the doctrine''s true operative content is judicial discretion dressed in historical method — theater_ratio would be underweighted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_analogue_test_methodology, empirical, 'Whether the doctrine''s central methodological tool is principled or outcome-driven.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_arms_right__individual_right_reading, 1791, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_arms_right__individual_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1900, second_amendment_arms_right__individual_right_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement_basis(seco_tr_t1900, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_arms_right__individual_right_reading, theater_ratio, 1939, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_arms_right__individual_right_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2010, second_amendment_arms_right__individual_right_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement_basis(seco_tr_t2010, observed).
narrative_ontology:measurement(seco_tr_t2022, second_amendment_arms_right__individual_right_reading, theater_ratio, 2022, 0.21).
narrative_ontology:measurement_basis(seco_tr_t2022, observed).
narrative_ontology:measurement(seco_tr_t2024, second_amendment_arms_right__individual_right_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(seco_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1791, 0.12).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1900, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1900, 0.15).
narrative_ontology:measurement_basis(seco_be_t1900, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_arms_right__individual_right_reading, base_extractiveness, 1939, 0.18).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2008, 0.3).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2010, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement_basis(seco_be_t2010, observed).
narrative_ontology:measurement(seco_be_t2022, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2022, 0.4).
narrative_ontology:measurement_basis(seco_be_t2022, observed).
narrative_ontology:measurement(seco_be_t2024, second_amendment_arms_right__individual_right_reading, base_extractiveness, 2024, 0.42).
narrative_ontology:measurement_basis(seco_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1900, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1900, 0.18).
narrative_ontology:measurement_basis(seco_su_t1900, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_arms_right__individual_right_reading, suppression_requirement, 1939, 0.2).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2008, 0.28).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2010, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2010, 0.3).
narrative_ontology:measurement_basis(seco_su_t2010, observed).
narrative_ontology:measurement(seco_su_t2022, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2022, 0.36).
narrative_ontology:measurement_basis(seco_su_t2022, observed).
narrative_ontology:measurement(seco_su_t2024, second_amendment_arms_right__individual_right_reading, suppression_requirement, 2024, 0.38).
narrative_ontology:measurement_basis(seco_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_arms_right__individual_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_arms_right__individual_right_reading, second_amendment_arms_right__civic_republican_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint stories instantiating readings of the second_amendment_arms_right kernel. The collective_right_reading (state militia authority, not individual ownership) and civic_republican_reading (armed citizenship as prerequisite for republican self-governance) are separate constraints with their own ε, beneficiary/victim sets, and classifications — per the ε-invariance principle, they are not measurement variants of this constraint but structurally distinct claims sharing a contested textual kernel. This reading (individual_right_reading) is authored with the highest expected ε of the three because its doctrinal application (Heller/McDonald/Bruen) most directly displaces contemporary regulatory authority; the sibling readings would each be authored independently with their own metrics rather than as adjustments to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
