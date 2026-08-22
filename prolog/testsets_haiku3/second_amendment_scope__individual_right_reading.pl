% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__individual_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__individual_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: second_amendment_scope__individual_right_reading
 *   human_readable: Second Amendment Individual Right Reading
 *   domain: constitutional_law/political_theory/rights_jurisprudence
 *
 * SUMMARY:
 *   The individual-right reading of the Second Amendment claims that the
 *   operative clause ('the right of the people to keep and bear Arms, shall
 *   not be infringed') protects an individual's right to firearm ownership
 *   for any lawful purpose — self-defense, sport, resistance to tyranny —
 *   independent of militia service. This reading frames all individuals as
 *   beneficiaries of the right and treats state regulatory authority as a
 *   victim (constrained by strict scrutiny). The reading rose to judicial
 *   salience with District of Columbia v. Heller (2008) and has hardened as a
 *   constitutional matter since. This story captures the individual-right
 *   reading AS IT STANDS, assessed by its own interpretive lights: the
 *   standing arrangement it describes is the constitutional protection of
 *   unqualified individual gun ownership rights, and extraction is measured
 *   relative to what regulatory authorities and public-health constituencies
 *   bear as a cost of that protection. The endorsed alternative (how a
 *   different reading would structure things) is NOT the referent of ε — per
 *   DP-001 (ε-invariance), ε measures the reading's own referent, not its
 *   alternative.
 *
 * KEY AGENTS:
 *   - Individual citizens (beneficiaries — possess unqualified right to own firearms)
 *   - Firearms manufacturers and dealers (beneficiaries — benefit from broad market access and minimal regulatory constraint)
 *   - Gun ownership advocates and civil rights organizations (beneficiaries — defend the reading and the right it protects)
 *   - Public health constituencies and gun violence communities (victims — bear the aggregate cost of widespread firearm access via injury/death rates, but constrained by the reading's protection)
 *   - State regulatory authorities (victims — constrained by strict scrutiny from regulating ammunition, licensing, storage, or narrow categories of firearm sales)
 *   - Collective-right and civic-right reading proponents (excluded from this story's consensus — their reading would reframe the entire structure)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, 0.68).
domain_priors:suppression_score(second_amendment_scope__individual_right_reading, 0.42).
domain_priors:theater_ratio(second_amendment_scope__individual_right_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(second_amendment_scope__individual_right_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__individual_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__individual_right_reading, "Second Amendment Individual Right Reading").
narrative_ontology:topic_domain(second_amendment_scope__individual_right_reading, "constitutional_law/political_theory/rights_jurisprudence").

domain_priors:requires_active_enforcement(second_amendment_scope__individual_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__individual_right_reading, '8766ebb1-59f0-4b41-9c7e-477fc019e79b').
narrative_ontology:cs_kernel_codification('8766ebb1-59f0-4b41-9c7e-477fc019e79b', fixed_text).
narrative_ontology:cs_authority_grounding('8766ebb1-59f0-4b41-9c7e-477fc019e79b', lineage).
narrative_ontology:cs_interpretation_layer_present('8766ebb1-59f0-4b41-9c7e-477fc019e79b').
narrative_ontology:cs_reading_relation('8766ebb1-59f0-4b41-9c7e-477fc019e79b', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_reading_relation('8766ebb1-59f0-4b41-9c7e-477fc019e79b', second_amendment_scope__civic_right_reading, influences).
narrative_ontology:cs_axiom('8766ebb1-59f0-4b41-9c7e-477fc019e79b', foundational, operative_clause_unqualified_by_militia).
narrative_ontology:cs_axiom_status(operative_clause_unqualified_by_militia, holdable).
narrative_ontology:cs_axiom_grounding('8766ebb1-59f0-4b41-9c7e-477fc019e79b', operative_clause_unqualified_by_militia, empirically_contingent).
narrative_ontology:cs_axiom('8766ebb1-59f0-4b41-9c7e-477fc019e79b', secondary, strict_scrutiny_bars_most_regulations).
narrative_ontology:cs_axiom_status(strict_scrutiny_bars_most_regulations, holdable).
narrative_ontology:cs_axiom_grounding('8766ebb1-59f0-4b41-9c7e-477fc019e79b', strict_scrutiny_bars_most_regulations, conventional).
narrative_ontology:cs_reference_frame('8766ebb1-59f0-4b41-9c7e-477fc019e79b', unqualified_individual_right_to_arms).
narrative_ontology:cs_drift_state('8766ebb1-59f0-4b41-9c7e-477fc019e79b', contemporary_public_health_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8766ebb1-59f0-4b41-9c7e-477fc019e79b', '').
narrative_ontology:cs_kernel_id(second_amendment_scope__individual_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, individual_citizens).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_scope__individual_right_reading, gun_ownership_advocates).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, public_health_constituencies).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, gun_violence_communities).
narrative_ontology:constraint_victim(second_amendment_scope__individual_right_reading, state_regulatory_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess the protected right to own firearms for lawful purposes. They benefit from the reading's broad scope and minimal regulatory constraint. They cannot exit the jurisdiction to escape the reading (though they can advocate for alternative readings). Their power is diffuse as individuals but organized through advocacy groups.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, individual_citizens, beneficiary,
    powerless, biographical, constrained, national).

% Operate under the protection of the individual-right reading, which shields them from ammunition bans, licensing mandates, and sales restrictions that would reduce their market. They benefit directly from broad consumer access. They have arbitrage options: can relocate to favorable jurisdictions or litigate unfavorable regulations.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, firearms_manufacturers, beneficiary,
    institutional, generational, mobile, national).

% Defend and promote the individual-right reading through litigation, legislation, and advocacy. They benefit from the reading's authority and expand its scope through case law. They have substantial exit options: they can shift to alternative strategies (state legislation, constitutional amendment) if the reading weakens.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_ownership_advocates, beneficiary,
    organized, generational, arbitrage, national).

% Is constrained by the individual-right reading from enacting ammunition restrictions, universal licensing, storage mandates, or narrow firearm bans. The reading's strict scrutiny standard requires them to justify regulations with compelling state interest — a high bar. They bear the cost of reduced regulatory capacity in public health matters.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, state_regulatory_authority, payer,
    institutional, generational, constrained, national).

% Bear the aggregate cost of the reading's broad protection: higher rates of gun violence, suicide, accidents, and homicide than in jurisdictions with more restrictive readings. They cannot exit the jurisdictions the reading governs. They can advocate for alternative readings or for narrowing exceptions (domestic violence, etc.) within the reading's frame.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, public_health_constituencies, payer,
    organized, biographical, constrained, national).

% Face the direct cost of the reading's protection: injury, death, community destabilization from firearm violence. They have minimal exit options and minimal power to influence the reading. They are diffuse and unorganized relative to gun owners and manufacturers.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, gun_violence_communities, payer,
    powerless, immediate, trapped, local).

% Would argue that the Second Amendment protects state militia authority, not individual ownership rights, and that the reading is a modern innovation inconsistent with founding-era intent. They are structurally excluded from the consensus this reading establishes — their interpretation is treated as legally settled against by the current doctrine.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, collective_right_reading_proponents, excluded,
    organized, generational, arbitrage, national).

% Would argue that the Second Amendment protects individual rights CONDITIONED on civic militia participation or training, splitting the difference between collective and individual readings. They are excluded from the individual-right consensus and would advance a reading that constrains the right's scope more than the individual reading but less than the collective reading.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, civic_right_reading_proponents, excluded,
    organized, generational, arbitrage, national).

% Analyze founding-era intent and constitutional text. They produce the corroborating analysis the individual-right reading depends on. They can shift findings if new historical evidence emerges, and they compete with other scholarly traditions (living constitutionalism, structural, democratic constitutionalism) on interpretive authority.
narrative_ontology:constraint_stakeholder(second_amendment_scope__individual_right_reading, originalist_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__individual_right_reading, firearms_manufacturers).
narrative_ontology:fixing_cost_class(second_amendment_scope__individual_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes that firearm ownership is an individual right, not a privilege the state may revoke, and thus coordinates expectation that citizens may possess firearms for lawful purposes (self-defense, sport, political expression) without governmental preapproval.
% TRANSFER_FUNCTION: Transfers regulatory discretion from legislative bodies (which might impose licensing, ammunition controls, or narrow firearm bans) to courts applying strict scrutiny, and transfers political capital from public-health advocates to gun-ownership advocates and manufacturers. Transfers the aggregate cost of unrestricted firearm access to public-health constituencies and gun-violence communities.
% ABSENT_VOICES: Collective-right and civic-right reading proponents are excluded — they would argue the reading misconstrues the militia clause and that the right should be conditioned or collective. Gun-violence communities (families of victims, public-health researchers) are structurally present but have minimal voice in the reading's authority structure (courts, originalist scholars, gun-rights organizations).
% DISAPPEARANCE_RATIONALE: If the individual-right reading disappeared overnight — if Supreme Court doctrine or constitutional amendment reversed it — the entire structure of gun regulation would reorganize: manufacturers would face new market constraints, states could impose licensing and ammunition controls, gun owners would lose their unqualified constitutional shield, and public-health constituencies would gain regulatory options. The political and legal landscape would shift materially.
% FOUNDING_PROBLEM: The Framers sought to protect the right of citizens to possess firearms for self-defense against private harms and to resist governmental tyranny — to preserve the means for a free people to defend liberty.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars (Heller-era scholarship by Randy Barnett, Eugene Volokh, others) corroborate the founding-intent claim by citing founding-era state constitutions and militia documents. Gun-ownership advocates cite contemporary crime rates and tyranny risks as evidence the problem remains live. Gun-violence public-health researchers and alternative-reading proponents argue that the founding problem (tyranny by centralized government, absence of professional police) is DEAD in modern democracies and that the reading persists due to institutional inertia and identity lock, not because the founding problem is live. No consensus corroboration outside the benefiting parties exists.
narrative_ontology:disappearance_verdict(second_amendment_scope__individual_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__individual_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__individual_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__individual_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_scope__individual_right_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__individual_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__individual_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__individual_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the individual-right reading PROTECTS broad firearm access, generating a large asymmetry: beneficiaries (all gun owners, manufacturers) collect the benefit of unrestricted rights, while victims (regulatory authorities, public-health constituencies) bear costs they cannot fully mitigate. The reading itself does NOT extract in the sense of a snare — it does not present a cover story for pure coercion. But it is tangled because it BOTH (1) protects genuine individual liberty (a coordination function: establishing that ownership is a right, not a privilege the state revokes) AND (2) blocks regulatory alternatives that would reduce aggregate harms (an asymmetric extraction). Suppression is lower (0.42) than extraction because the reading's persistence does not depend primarily on coercion or hiding alternatives — it depends on a coherent interpretive case grounded in text, history, and judicial authority. However, suppression is not negligible because the reading's survival also depends on limiting public visibility of gun-violence data and on maintaining identity fusion with gun owners (internalized suppression, per the identity_lock omega). Theater ratio is moderate (0.28) — there is real constitutional discourse and textual exegesis, but an increasing share of institutional activity is defensive (resisting reinterpretation) rather than affirmative (advancing new interpretation).
 *
 * PERSPECTIVAL GAP:
 *   The structural divergence between the beneficiary and victim seats should be stark. From the beneficiary seat (gun owner, manufacturer), the reading is a protection of fundamental liberty — no extraction narrative applies; the owner simply possesses a right. From the regulatory authority seat (victim), the reading is a constraint on their power to protect public health, and they bear the cost of that constraint. From the gun-violence community seat, the reading extracts lives and health. The engine should compute these as sharply divergent types when populated with the same structural facts. The perspectival gap is not a failure — it is the design signal: a reading that is liberty-protecting from one seat and extractive from another reveals the reading's asymmetric structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Individual citizens and manufacturers are beneficiaries (d near 0.0): they collect the protected right with minimal constraint. State regulatory authorities are victims (d near 1.0): the reading's strict scrutiny standard blocks most regulatory alternatives and transfers discretion to courts (away from legislatures). Public-health constituencies are also victims but with higher exit options (d ~0.7): they cannot exit the jurisdictions the reading governs, but they can organize politically for constitutional amendment or can advocate in the sibling readings. The beneficiary/victim split is driven entirely by the reading's assignment of the protected right to individuals and its constraint on state authority — change the reading, and the directionalities reverse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (what the right was meant to protect) was stated as the need for a free people to resist tyranny and defend themselves against government and private harm. That problem is LIVE in the eyes of the reading's beneficiaries (gun owners cite ongoing tyranny risk and crime) but is CONTESTED or DEAD in the eyes of victims and alternative readings (modern democracies have police, courts, and are not tyrannies). The reading itself does not resolve mandatrophy — it depends on whether you believe the founding problem is still the live threat its proponents claim. This is a genuine kernel-level disagreement, not a constraint that has outlived its function. The reading persists because it has institutional authority (Supreme Court precedent) and identity lock, not because it is theatrically maintained or zombified. Classification as tangled_rope (rather than piton) is appropriate: the reading genuinely coordinates a protection (individual liberty in arms) AND extracts asymmetrically (from regulatory capacity and public-health authorities).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    well_regulated_militia_clause_scope,
    'Does ''well regulated Militia'' function as a prefatory clause limiting the operative clause to militia-related purposes, or is it merely historical context leaving the operative right unqualified?',
    'Originalist textual analysis combined with founding-era state militia documents and contemporary constitutional scholarship across reading traditions. The foundational disagreement between readings hinges on this interpretive choice.',
    'Individual-right reading assigns the clause minimal limiting force; collective-right reading assigns it maximal scope-limiting effect; civic-right reading treats it as conditioning clause. This single interpretive choice produces different ε values across readings, different beneficiary/victim structures, and different constraint classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(well_regulated_militia_clause_scope, conceptual, 'Interpretive scope of the militia prefatory clause — the core structural disagreement between the three readings of this kernel.').

omega_variable(
    historical_consensus_on_founding_intent,
    'Did the Framers intend to protect an individual right to firearm ownership for self-defense, or a state''s right to maintain militias, or a conditioned right tied to civic militia participation?',
    'Founding-era documents (ratification debates, state constitutions, Framers'' papers), militia membership and training patterns 1787-1791, and legal historians'' forensic analysis. The three readings advance conflicting corroborations from the same historical record.',
    'High confidence in a founding intent claim would stabilize the kernel within one reading''s reference frame and possibly foreclose others; ambiguity or contested evidence sustains coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_consensus_on_founding_intent, empirical, 'What did the Framers intend to protect — the historical question that grounds all three readings.').

omega_variable(
    modern_firearm_lethality_and_historical_context,
    'Does the availability of weapons far more lethal than those extant in 1791 require the individual-right reading to be bounded by findings about public safety necessity in a way inconsistent with its originalist framing?',
    'Constitutional doctrine on scope-limiting factors (intermediate scrutiny, regulatory tailoring, public health exceptions in other contexts) combined with empirical data on firearm lethality expansion and accident/suicide/homicide rates.',
    'If modern weapons require substantial regulatory boundaries even under individual-right framing, the reading''s practical extractiveness (scope narrowing) increases; if the reading''s originalist logic forbids such boundaries, the ε remains high but faces pressure from competing public-safety readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_firearm_lethality_and_historical_context, empirical, 'Whether modern weapon lethality triggers re-reading of the historical right''s scope.').

omega_variable(
    identity_lock_gun_owners_reading_survival,
    'For individuals whose political, professional, or personal identity is constituted through gun ownership and the individual-right reading, how much would abandoning this reading destabilize their self-concept?',
    'Ethnographic documentation of gun-owner communities, identity fusion research (how gun rights tie to militia self-concept, frontier narrative, rural independence, personal security identity), and comparison to exit-option behavior if this reading were reversed.',
    'High identity lock would make the reading''s persistence path inertial rather than epistemic — the reading would persist because agents have fused their identity to it, not because its corroboration is strongest. This is a suppression mechanism (internalized, identity-based) that the low suppression score (0.42) does not fully capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_gun_owners_reading_survival, conceptual, 'Identity fusion with the individual-right reading — suppression mechanism not fully captured by the structural suppression metric.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__individual_right_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__individual_right_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(seco_tr_t10, second_amendment_scope__individual_right_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement(seco_tr_t20, second_amendment_scope__individual_right_reading, theater_ratio, 20, 0.25).
narrative_ontology:measurement(seco_tr_t30, second_amendment_scope__individual_right_reading, theater_ratio, 30, 0.28).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__individual_right_reading, base_extractiveness, 0, 0.54).
narrative_ontology:measurement(seco_be_t10, second_amendment_scope__individual_right_reading, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(seco_be_t20, second_amendment_scope__individual_right_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement(seco_be_t30, second_amendment_scope__individual_right_reading, base_extractiveness, 30, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__individual_right_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(seco_su_t10, second_amendment_scope__individual_right_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(seco_su_t20, second_amendment_scope__individual_right_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(seco_su_t30, second_amendment_scope__individual_right_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__individual_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__individual_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__collective_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__individual_right_reading, second_amendment_scope__civic_right_reading).

% DUAL FORMULATION NOTE:
% The Second Amendment scope kernel admits three structurally distinct readings: individual_right_reading (this story, protects unqualified individual ownership), collective_right_reading (protects state militia authority), and civic_right_reading (protects individual rights conditioned on militia participation). Each reading assigns different ε values, different beneficiary/victim structures, and produces different constraint types. They are not views on one constraint; they are three constraints arising from one contested text. All three must be authored separately and linked via network.affects_constraints to form the kernel family. This reading (individual_right) influences the others by establishing a constitutional precedent that competing readings must address.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
