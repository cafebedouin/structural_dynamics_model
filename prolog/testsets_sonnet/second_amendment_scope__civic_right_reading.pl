% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment as Civic-Republican Right (Militia-Conditioned Reading)
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the civic-right reading of the Second Amendment
 *   kernel: the position that the constitutional protection for keeping and
 *   bearing arms is conditioned on a nexus to organized militia service,
 *   reading the prefatory clause ('A well regulated Militia, being necessary
 *   to the security of a free State') as operative rather than merely
 *   explanatory. Under this reading, the Amendment's core beneficiaries are
 *   militia-eligible citizens and the state militia structures themselves,
 *   and non-militia personal ownership sits at the doctrine's periphery,
 *   subject to greater regulatory latitude. The extractiveness (0.42)
 *   reflects moderate service-based gating: the reading does not eliminate
 *   individual protection, but it structurally advantages those who can claim
 *   or maintain a civic/militia nexus over those who cannot, and it
 *   authorizes a real body of regulation aimed at the latter group. This is a
 *   distinct constraint from the individual_right_reading (which would show
 *   much lower extraction toward non-militia owners because it grants them
 *   full, unconditioned protection) and from the collective_right_reading
 *   (which would show near-total extraction toward all individual claimants
 *   because it recognizes no individual right at all). The three readings are
 *   not the same constraint measured differently — they are three constraints
 *   with three different beneficiary/victim structures and three different ε
 *   values, linked as siblings of the second_amendment_scope kernel.
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: Primary beneficiary (moderate/constrained) — retains protection via civic nexus
 *   - organized_state_militias: Institutional beneficiary and agenda-setter (institutional/constrained) — administers the qualifying structure
 *   - non_militia_gun_owners: Primary target (moderate/constrained) — bears reduced constitutional protection
 *   - urban_residents_seeking_self_defense_only_ownership: Most exposed target (powerless/trapped) — least able to claim the civic predicate or exit restrictive jurisdictions
 *   - civic_republican_legal_scholars: Analytical/advocacy beneficiary (organized/mobile) — advances the doctrine without bearing its costs
 *   - federal_and_state_courts: Analytical observer (institutional/analytical) — adjudicates among the sibling readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.42).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.38).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment as Civic-Republican Right (Militia-Conditioned Reading)").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, 'c809760f-00b5-4b37-9fe8-4028cc19adb8').
narrative_ontology:cs_kernel_codification('c809760f-00b5-4b37-9fe8-4028cc19adb8', fixed_text).
narrative_ontology:cs_authority_grounding('c809760f-00b5-4b37-9fe8-4028cc19adb8', lineage).
narrative_ontology:cs_interpretation_layer_present('c809760f-00b5-4b37-9fe8-4028cc19adb8').
narrative_ontology:cs_reading_relation('c809760f-00b5-4b37-9fe8-4028cc19adb8', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('c809760f-00b5-4b37-9fe8-4028cc19adb8', second_amendment_scope__collective_right_reading, influences).
narrative_ontology:cs_axiom('c809760f-00b5-4b37-9fe8-4028cc19adb8', foundational, prefatory_clause_is_operative_condition).
narrative_ontology:cs_axiom_status(prefatory_clause_is_operative_condition, holdable).
narrative_ontology:cs_axiom_grounding('c809760f-00b5-4b37-9fe8-4028cc19adb8', prefatory_clause_is_operative_condition, conventional).
narrative_ontology:cs_axiom('c809760f-00b5-4b37-9fe8-4028cc19adb8', foundational, individual_right_survives_but_is_gated_by_civic_nexus).
narrative_ontology:cs_axiom_status(individual_right_survives_but_is_gated_by_civic_nexus, holdable).
narrative_ontology:cs_axiom_grounding('c809760f-00b5-4b37-9fe8-4028cc19adb8', individual_right_survives_but_is_gated_by_civic_nexus, deontological).
narrative_ontology:cs_reference_frame('c809760f-00b5-4b37-9fe8-4028cc19adb8', founding_era_organized_militia_system).
narrative_ontology:cs_drift_state('c809760f-00b5-4b37-9fe8-4028cc19adb8', post_national_guard_federalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c809760f-00b5-4b37-9fe8-4028cc19adb8', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, organized_state_militias).
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, civic_republican_legal_scholars).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_gun_owners).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, urban_residents_seeking_self_defense_only_ownership).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republican_theory_of_arms_bearing).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, well_regulated_militia_clause_operative_force).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who satisfy the civic-participation predicate (historically: able-bodied citizens subject to militia call-up, today read as those enrolled in or eligible for organized state militia service) retain a clear, protected right to keep and bear arms under this reading. Their protection is not contingent on a separate showing of self-defense need — participation in the civic structure is itself the qualifying condition.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    moderate, generational, constrained, national).

% State-organized militia structures (historically the state militia system, in modern doctrine analogized to National Guard units or state defense forces) are the institutional anchor this reading treats as the Amendment's actual subject. They administer eligibility, training, and call-up, and their continued existence is what gives the right its content and its limits.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, organized_state_militias, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, organized_state_militias, agenda_setter).

% Individuals who own firearms purely for personal self-defense, hunting, or collection, with no militia nexus, find their claim to constitutional protection weakened under this reading — regulation aimed at non-militia ownership is easier to sustain because the civic predicate is unmet. They bear the practical cost of a doctrine that treats their ownership as outside the Amendment's core concern, even though they are not organized enough to contest the framing collectively.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_gun_owners, payer,
    moderate, biographical, constrained, national).

% Residents of jurisdictions with restrictive firearm regimes who seek to own arms strictly for home or personal defense, with no plausible militia service claim, find the civic-right reading offers them the least constitutional cover of the three readings. They cannot exit the jurisdiction's regulatory regime without relocating, and cannot manufacture a militia nexus they do not have.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, urban_residents_seeking_self_defense_only_ownership, payer,
    powerless, biographical, trapped, local).

% Academics and jurists advancing the civic-republican reading gain intellectual and professional standing when courts or legislatures adopt their framework; they administer the doctrine's elaboration through scholarship, amicus briefs, and judicial appointments advocacy, and can shift their arguments as new cases arise without bearing the practical costs imposed on regulated individuals.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, civic_republican_legal_scholars, beneficiary,
    organized, civilizational, mobile, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, civic_republican_legal_scholars, agenda_setter).

% Advocates of the unconnected individual-right reading are structurally positioned outside this reading's framework — the civic-right reading's own logic treats their preferred interpretation as historically and textually incorrect. They would object that this reading strips the Amendment of independent force for ordinary citizens, but their objection operates in a rival doctrinal camp rather than within this reading's own terms.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, individual_rights_gun_owners_coalition, excluded,
    organized, generational, constrained, national).

% Courts adjudicate which reading controls in a given case, drawing on historical evidence about militia practice, founding-era firearm regulation, and the structure of the constitutional text. Their choice among readings determines which stakeholders' claims receive protection and which regulatory schemes survive review.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_and_state_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ties the constitutional protection for arms-bearing to participation in an organized, state-administered militia structure, coordinating the individual's right with a collective defense function the framers understood as essential to resisting standing-army tyranny and providing common defense without a large peacetime military.
% TRANSFER_FUNCTION: Moves constitutional protection toward those with a militia nexus (organized militia members, militia-eligible citizens) and away from those whose firearm ownership is untethered to civic/military service; correspondingly shifts regulatory latitude toward legislatures regulating non-militia ownership.
% ABSENT_VOICES: Advocates of the unconnected individual-right reading are excluded from this reading's own interpretive frame — they would argue the civic predicate reads a limitation into the text the framers did not intend, but that argument belongs to a rival reading, not a challenge internal to this one. Historically, militia members of color and women were frequently excluded from actual militia rolls despite the theoretical universality of the civic predicate — a genealogical tension this reading does not fully resolve.
% DISAPPEARANCE_RATIONALE: If this specific reading vanished as the controlling doctrine, states would lose the strongest doctrinal basis for firearm regulations aimed at non-militia ownership, and litigation would shift decisively toward whichever sibling reading filled the vacuum — the individual-right reading would likely expand protection for unconnected ownership, while the collective-right reading would narrow protection further toward pure state authority. Whether 'the world rearranges' depends entirely on which sibling reading is adopted in its place, which is itself contested.
% FOUNDING_PROBLEM: The Second Amendment was drafted amid anxiety about standing armies as instruments of tyranny and a felt need to preserve state militia capacity for common defense, integrating an armed, civically-obligated citizenry into the constitutional structure as a check on federal military power.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the founding era and originalist scholars outside the civic-republican camp (including some individual-right proponents) acknowledge the militia-anxiety context as genuine founding-era concern, but dispute whether it operates as a textual condition on the right today given the dissolution of the founding-era militia system into the modern National Guard. No consensus corroboration exists from outside the interpretive camps that benefit from each respective reading; the corroboration is itself doctrinally contested.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__civic_right_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).
:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because this reading still recognizes an individual right — it merely conditions it on civic participation, so those with a plausible militia nexus retain meaningful protection; the extraction falls on the growing population whose ownership has no service dimension. Suppression (0.38) is likewise moderate: the reading does not suppress ownership outright, but the enforcement of the civic predicate (determining who qualifies, sustaining regulatory schemes for non-qualifying owners) requires active judicial and legislative maintenance. Theater ratio (0.28) reflects that the modern militia nexus is largely notional — actual state militia rolls and call-up practice bear little resemblance to founding-era service, so a portion of the doctrinal apparatus performs a civic-participation story that has limited operative correlate. Resistance (0.62) is high because both individual-rights advocates and public-safety advocates actively contest this middle reading from opposite directions, and accessibility_collapse (0.45) is moderate — the sibling readings remain fully live doctrinal alternatives, so no single reading has foreclosed the interpretive field.
 *
 * DIRECTIONALITY LOGIC:
 *   Militia-eligible citizens and organized state militias sit near the beneficiary end: the doctrine's core protection is built around their claim. Civic-republican scholars benefit reputationally and professionally without bearing the doctrine's costs, giving them a beneficiary/agenda-setter dual role. Non-militia gun owners and, more acutely, urban residents seeking self-defense-only ownership sit toward the target end: the civic predicate structurally disadvantages their claims relative to the individual-right reading, and their exit options (relocating jurisdictions, manufacturing a militia nexus) are limited or trapped for the most exposed group.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — standing-army anxiety and the need for organized civic defense — is genuinely contested as live or dead: professional standing militias have been superseded by the National Guard and a large peacetime federal military, which could be read as mooting the founding predicate entirely (founding_problem_status: contested rather than flatly dead, since some scholars argue the civic-check function persists in attenuated form). This reading avoids mislabeling the doctrine as pure extraction by preserving genuine individual protection for those with the civic nexus — it is not a bare assertion of state power over disarmed citizens (that would be the collective_right_reading). But it also avoids treating the doctrine as costless coordination: it names concrete victims (non-militia owners) who bear real doctrinal disadvantage, which is why tangled_rope rather than rope or mountain is the structurally honest claim here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civic_right_reading_sibling_disagreement_location,
    'Where exactly do the three kernel readings diverge, and is the divergence resolvable by historical evidence or is it an irreducibly normative choice about how to read the prefatory clause?',
    'The disagreement is located at the operative-vs-explanatory status of the prefatory militia clause. Individual_right_reading treats it as non-limiting historical explanation; civic_right_reading (this constraint) treats it as an operative condition; collective_right_reading treats the entire right as belonging to states, not individuals. Historical linguistics and founding-era legislative practice bear on this but have not produced scholarly consensus after decades of debate, suggesting the disagreement has an irreducible normative-interpretive component alongside its empirical one.',
    'If resolved toward individual_right_reading, this constraint''s beneficiary set (militia-eligible citizens) loses its distinguishing structural advantage and the reading collapses into the sibling; if resolved toward collective_right_reading, the individual-protection component this reading preserves disappears entirely. The reading''s survival as a distinct doctrinal position depends on maintaining a middle ground that neither sibling accepts.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_right_reading_sibling_disagreement_location, conceptual, 'Committer-frame omega: names the kernel, this reading, and the specific site of interpretive disagreement among siblings.').

omega_variable(
    modern_militia_nexus_hollowness,
    'Does a genuine civic-participation predicate exist in the modern era (given the transformation of state militias into the National Guard, a federally-integrated force), or has the civic nexus become a purely notional gatekeeping device with no operative content?',
    'Empirical examination of whether any modern legal doctrine actually requires or verifies militia-type service as a precondition for firearm rights in any jurisdiction that has adopted a civic-right framework, versus whether the civic-nexus language functions purely as rhetorical constraint on regulatory latitude.',
    'If the nexus is genuinely hollow (no jurisdiction actually gates rights on verified militia service), the theater_ratio for this reading should be substantially higher than authored, and the reading may function closer to a scaffold that lost its sunset than to a live tangled_rope with authentic coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_militia_nexus_hollowness, empirical, 'Whether the civic-participation gating mechanism has operative content or is purely theatrical.').

omega_variable(
    founding_era_militia_exclusion_genealogy,
    'Given that founding-era militia rolls systematically excluded enslaved people, free Black men in many jurisdictions, and women, does the civic-right reading inherit and formalize that exclusionary genealogy, or can the civic predicate be read as race/gender-neutral in principle even if historically applied unevenly?',
    'Historical research into whether founding-era arguments for the civic-right reading depended constitutively on a racially/gender-restricted conception of citizenship-eligible-for-militia-service, versus whether the predicate was understood as universal in principle and merely applied with the era''s general exclusions.',
    'If the predicate was constitutively exclusionary, applying it today without acknowledgment risks smuggling historical exclusion into a facially neutral modern doctrine, which would raise the effective suppression/extraction for groups historically barred from the militia-eligible category.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_era_militia_exclusion_genealogy, conceptual, 'Whether the civic predicate carries an inherited exclusionary genealogy from founding-era militia composition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 1791, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t1791, second_amendment_scope__civic_right_reading, theater_ratio, 1791, 0.1).
narrative_ontology:measurement_basis(seco_tr_t1791, observed).
narrative_ontology:measurement(seco_tr_t1870, second_amendment_scope__civic_right_reading, theater_ratio, 1870, 0.15).
narrative_ontology:measurement_basis(seco_tr_t1870, observed).
narrative_ontology:measurement(seco_tr_t1939, second_amendment_scope__civic_right_reading, theater_ratio, 1939, 0.2).
narrative_ontology:measurement_basis(seco_tr_t1939, observed).
narrative_ontology:measurement(seco_tr_t1980, second_amendment_scope__civic_right_reading, theater_ratio, 1980, 0.22).
narrative_ontology:measurement_basis(seco_tr_t1980, observed).
narrative_ontology:measurement(seco_tr_t2008, second_amendment_scope__civic_right_reading, theater_ratio, 2008, 0.26).
narrative_ontology:measurement_basis(seco_tr_t2008, observed).
narrative_ontology:measurement(seco_tr_t2025, second_amendment_scope__civic_right_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(seco_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t1791, second_amendment_scope__civic_right_reading, base_extractiveness, 1791, 0.2).
narrative_ontology:measurement_basis(seco_be_t1791, observed).
narrative_ontology:measurement(seco_be_t1870, second_amendment_scope__civic_right_reading, base_extractiveness, 1870, 0.25).
narrative_ontology:measurement_basis(seco_be_t1870, observed).
narrative_ontology:measurement(seco_be_t1939, second_amendment_scope__civic_right_reading, base_extractiveness, 1939, 0.3).
narrative_ontology:measurement_basis(seco_be_t1939, observed).
narrative_ontology:measurement(seco_be_t1980, second_amendment_scope__civic_right_reading, base_extractiveness, 1980, 0.34).
narrative_ontology:measurement_basis(seco_be_t1980, observed).
narrative_ontology:measurement(seco_be_t2008, second_amendment_scope__civic_right_reading, base_extractiveness, 2008, 0.4).
narrative_ontology:measurement_basis(seco_be_t2008, observed).
narrative_ontology:measurement(seco_be_t2025, second_amendment_scope__civic_right_reading, base_extractiveness, 2025, 0.42).
narrative_ontology:measurement_basis(seco_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t1791, second_amendment_scope__civic_right_reading, suppression_requirement, 1791, 0.15).
narrative_ontology:measurement_basis(seco_su_t1791, observed).
narrative_ontology:measurement(seco_su_t1870, second_amendment_scope__civic_right_reading, suppression_requirement, 1870, 0.2).
narrative_ontology:measurement_basis(seco_su_t1870, observed).
narrative_ontology:measurement(seco_su_t1939, second_amendment_scope__civic_right_reading, suppression_requirement, 1939, 0.25).
narrative_ontology:measurement_basis(seco_su_t1939, observed).
narrative_ontology:measurement(seco_su_t1980, second_amendment_scope__civic_right_reading, suppression_requirement, 1980, 0.28).
narrative_ontology:measurement_basis(seco_su_t1980, observed).
narrative_ontology:measurement(seco_su_t2008, second_amendment_scope__civic_right_reading, suppression_requirement, 2008, 0.35).
narrative_ontology:measurement_basis(seco_su_t2008, observed).
narrative_ontology:measurement(seco_su_t2025, second_amendment_scope__civic_right_reading, suppression_requirement, 2025, 0.38).
narrative_ontology:measurement_basis(seco_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language label 'the Second Amendment right' per the ε-invariance principle. civic_right_reading (this file) authors moderate ε (0.42) with service-based gating and a beneficiary set of militia-eligible citizens plus organized state militias, victims among non-militia owners. individual_right_reading would author substantially lower ε toward non-militia owners (they would be beneficiaries, not victims, under that reading) and different victim exposure (regulatory bodies attempting to restrict unconnected ownership face higher resistance). collective_right_reading would author much higher ε toward all individual claimants (near-total, since no individual right is recognized) with states/regulatory authorities as the sole beneficiaries. The three are linked via affects_constraints rather than merged because each has a distinct, stable, non-negotiable ε — attempting to average or parametrize across them would violate the ε-invariance test.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
