% ============================================================================
% CONSTRAINT STORY: second_amendment_text__originalist_civic_virtue_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__originalist_civic_virtue_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_text__originalist_civic_virtue_reading
 *   human_readable: Second Amendment Originalist Civic Virtue Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the originalist civic virtue reading of the
 *   Second Amendment: the Founders understood 'the militia' not as a
 *   state-organized institution but as the universal armed citizenry
 *   functioning as a distributed political check on tyranny. The right
 *   protects citizen-soldier capacity — the ability of the political
 *   community to maintain armed readiness — rather than personal self-defense
 *   or state-regulated collective security. This reading sits between two
 *   sibling readings: the collective security reading (militia clause
 *   conditions the right; state may regulate) and the individual right
 *   reading (operative clause guarantees individual right independent of
 *   militia service). The originalist reading shares with the individual
 *   reading a broad protected zone of firearm access, but grounds it in civic
 *   republican political theory rather than personal autonomy; it shares with
 *   the collective security reading an institutional frame (militia), but
 *   understands the institution as distributed and universal rather than
 *   state-controlled. The constraint describes a genuine coordination
 *   function — dispersed armed citizens as a structural safeguard against
 *   tyranny — but the measured extractiveness reflects the gap between that
 *   framing and the exclusion of voices (public health, progressive scholars)
 *   who experience the same legal structure as imposing costs without
 *   corresponding political benefit.
 *
 * KEY AGENTS:
 *   - Armed citizenry as political community: the bearer of the right, understood collectively as a civic militia; benefits from the reading by securing a normative frame that ties firearm access to political participation rather than individual risk management.
 *   - Originalist constitutional interpreters: institutional agenda-setters who establish and defend this reading through legal authority, scholarship, and judicial opinion.
 *   - Firearm rights advocacy organizations: mobilize around this reading to defend firearm access by leveraging the civic republican framing.
 *   - Progressive constitutional scholars and public health authorities: excluded from the consensus presupposed by this reading; would contest the founding problem status and the extraction-free assumptions.
 *   - Historical interpretation community: provides evidence about founding-era practice and intent, though does not adjudicate the constitutional question.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__originalist_civic_virtue_reading, 0.31).
domain_priors:suppression_score(second_amendment_text__originalist_civic_virtue_reading, 0.22).
domain_priors:theater_ratio(second_amendment_text__originalist_civic_virtue_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(second_amendment_text__originalist_civic_virtue_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__originalist_civic_virtue_reading, rope).
narrative_ontology:human_readable(second_amendment_text__originalist_civic_virtue_reading, "Second Amendment Originalist Civic Virtue Reading").
narrative_ontology:topic_domain(second_amendment_text__originalist_civic_virtue_reading, "constitutional_law/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__originalist_civic_virtue_reading, 'e11a9f2d-873d-4078-ae3d-68e094f4930b').
narrative_ontology:cs_kernel_codification('e11a9f2d-873d-4078-ae3d-68e094f4930b', fixed_text).
narrative_ontology:cs_authority_grounding('e11a9f2d-873d-4078-ae3d-68e094f4930b', lineage).
narrative_ontology:cs_interpretation_layer_present('e11a9f2d-873d-4078-ae3d-68e094f4930b').
narrative_ontology:cs_reading_relation('e11a9f2d-873d-4078-ae3d-68e094f4930b', second_amendment_text__collective_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('e11a9f2d-873d-4078-ae3d-68e094f4930b', second_amendment_text__individual_right_reading, coexists_with).
narrative_ontology:cs_axiom('e11a9f2d-873d-4078-ae3d-68e094f4930b', foundational, universal_militia_as_armed_citizenry).
narrative_ontology:cs_axiom_status(universal_militia_as_armed_citizenry, holdable).
narrative_ontology:cs_axiom_grounding('e11a9f2d-873d-4078-ae3d-68e094f4930b', universal_militia_as_armed_citizenry, empirically_contingent).
narrative_ontology:cs_axiom('e11a9f2d-873d-4078-ae3d-68e094f4930b', foundational, civic_virtue_check_on_tyranny).
narrative_ontology:cs_axiom_status(civic_virtue_check_on_tyranny, holdable).
narrative_ontology:cs_axiom_grounding('e11a9f2d-873d-4078-ae3d-68e094f4930b', civic_virtue_check_on_tyranny, deontological).
narrative_ontology:cs_reference_frame('e11a9f2d-873d-4078-ae3d-68e094f4930b', founding_era_civic_republicanism).
narrative_ontology:cs_drift_state('e11a9f2d-873d-4078-ae3d-68e094f4930b', contemporary_2024, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e11a9f2d-873d-4078-ae3d-68e094f4930b', '').
narrative_ontology:cs_kernel_id(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_as_political_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(second_amendment_text__originalist_civic_virtue_reading, firearm_rights_advocacy_organizations).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, civic_republicanism).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, founders_original_intent).
narrative_ontology:constraint_vindicates(second_amendment_text__originalist_civic_virtue_reading, universal_militia_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The reading frames the citizenry collectively as the bearer of the protected right: private citizens capable of bearing arms collectively constitute the militia, which is the check on tyranny. The benefit is framed as political empowerment through armed capacity, not personal self-defense. Citizens retain access to functional firearms as members of a political body, not as isolated individuals.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, armed_citizenry_as_political_community, beneficiary,
    organized, generational, mobile, national).

% Judges, scholars, and constitutional lawyers who adopt originalist method and argue that the founding-era understanding of militia as universal armed citizenry should govern modern interpretation. They set the interpretive frame through legal argument, judicial opinion, and academic authority.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, originalist_constitutional_interpreters, agenda_setter,
    institutional, generational, analytical, national).

% Organizations that mobilize around this reading to defend and expand firearm access. They benefit from the framing that ties the right to civic republicanism rather than personal self-defense, as it shifts the normative center from individual risk management to political participation.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, firearm_rights_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Scholars and advocates who reject originalist method or who interpret the Founders' intent differently (collective security reading, individual right reading). They are excluded from the consensus this reading presupposes, though they participate actively in the interpretive dispute.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, progressive_constitutional_scholars, excluded,
    institutional, generational, analytical, national).

% Public health bodies charged with reducing firearm mortality. They would argue that the civic republican framing obscures individual-level harms and constrains the state's capacity to regulate for collective safety. Their objections are structural, not merely advisory.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, public_health_authorities, excluded,
    institutional, biographical, analytical, national).

% Academic historians and historical scholars who evaluate competing claims about founding-era militia practice, organization, and the Founders' understanding. They provide evidence and interpretation that sibling readings rely on, though they do not adjudicate the constitutional question itself.
narrative_ontology:constraint_stakeholder(second_amendment_text__originalist_civic_virtue_reading, historical_interpretation_community, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Articulates a coordinated political response to the risk of tyranny by framing private firearm ownership as participation in a distributed civic militia. The coordination problem is preventing tyrannical concentration of force; the solution is universal armed readiness among the citizenry, not professional standing armies alone.
% TRANSFER_FUNCTION: Transfers interpretive authority from competing readings (collective security, individual right) to a frame that privileges founding-era historical context. The interpretive transfer flows from the political community (as organized citizenry) to originalist institutional interpreters (judges and scholars) who validate the reading through legal authority.
% ABSENT_VOICES: Public health authorities and victims of firearm violence are structurally excluded from the interpretive frame. The reading's core premise (civic virtue through armed readiness) presupposes that political community benefit outweighs individual-level harms, which those voices would contest. Progressive constitutional scholars are excluded from the consensus but participate in the legal dispute; public health experts are excluded more completely from the constitutional argument itself.
% DISAPPEARANCE_RATIONALE: If this reading disappeared and either the collective security reading or individual right reading dominated constitutional interpretation, the range of firearms deemed constitutionally protected might narrow (collective security) or expand (individual right); the normative valence of the right would shift from political participation to security regulation or personal liberty. The constraint would not vanish — the Second Amendment text would remain — but its binding interpretation would change materially.
% FOUNDING_PROBLEM: The Founders understood the newly constituted republic as vulnerable to tyranny through standing armies under executive control. A distributed armed citizenry, understood as a universal militia, provided a check on that risk without requiring permanent professional military structures that could themselves become instruments of tyranny.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars cite Founding-era texts (The Federalist, state constitutional provisions, militia statutes) as evidence the founding problem was a real strategic concern. Historians of the Revolutionary War and founding debates attest to the centrality of militia-based defense. However, progressive scholars contest the historical reading: they argue that militia was understood as organized state militia, not universal armed citizenry, and that the founding problem has been substantially transformed by professional standing militaries and international security arrangements. The corroboration exists but is contested across the interpretive community.
narrative_ontology:disappearance_verdict(second_amendment_text__originalist_civic_virtue_reading, contested).
narrative_ontology:founding_problem_status(second_amendment_text__originalist_civic_virtue_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__originalist_civic_virtue_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_text__originalist_civic_virtue_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__originalist_civic_virtue_reading_tests).
:- end_tests(second_amendment_text__originalist_civic_virtue_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.31 at interval end) because the reading articulates a genuine coordination function — distributed armed readiness as a structural check on tyranny — and does not require coercive enforcement of compliance. The beneficiary (armed citizenry qua political community) and the reading's institutional promoters (originalist interpreters) both benefit from the frame without suppressing alternatives through legal force. Suppression is low (0.22) because the reading operates primarily through interpretive authority and scholarly consensus, not through exclusionary enforcement. Theater ratio is low (0.18) and stable because the civic republican narrative is the core function, not performative maintenance of a degraded arrangement. Accessibility collapse is high (0.72) because once the reading is understood, the alternative interpretations are difficult to articulate within its frame — the civic virtue narrative pre-empts other normative considerations. Resistance is moderate-to-high (0.58) because public health advocates, progressive scholars, and those who experience firearm violence contest this reading vigorously. The measurement series shows extractiveness rising modestly in the early interval (originalist interpretation becoming more institutionalized in courts after 2008), then stabilizing once the reading achieves substantial judicial acceptance. Theater remains low throughout, consistent with a reading that is lived as a foundational interpretive principle, not as a maintained facade.
 *
 * PERSPECTIVAL GAP:
 *   Different seats compute this constraint differently. From the originalist interpreter's seat (institutional, analytical), the constraint is a pure coordination mechanism — the rediscovery of founding intent that clarifies a real political safeguard. From the armed citizen's seat (organized), the constraint provides political empowerment and a normative shield for access to functional firearms. From the excluded public health authority's seat (institutional, analytical), the same constraint operates as a legal barrier to regulation — it extracts political sovereignty from the health system to the gun-owning community. From the victim of firearm violence's seat (not directly represented in the stakeholder set), the constraint would appear extractive in a fundamentally different way: it transfers the benefit of armed readiness to a political community while concentrating harms on individuals outside that frame. The engine should compute the originalist interpreter as near-beneficiary (d ~0.15-0.25), the armed citizenry as beneficiary (d ~0.1-0.2), the excluded public health authority as a target (d ~0.75-0.85), and the political community as a whole as ambiguous (d ~0.5) — split between those who identify with civic republicanism and those who do not.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality depends on whether an agent views the Founders' founding problem as still live and whether they identify with the political community's armed readiness as a benefit. Originalist interpreters benefit from the reading (institutional authority, professional legitimacy in legal academia). The armed citizenry benefits by gaining a normative frame that ties firearm access to political participation rather than personal self-defense, which shifts the default toward broader access. Public health authorities and violence-prevention advocates are targets: the reading constrains their regulatory authority by tying rights protection to historical intent rather than contemporary outcomes. Progressive scholars are targets in a softer sense: they must defend alternative readings against originalist authority, which is an asymmetric interpretive burden. The political community is split: those who identify with civic republicanism benefit; those who do not identify with that frame or who experience arms differently (as threats rather than safeguards) bear costs. No directionality override is necessary because the structural data suffice — the beneficiary declaration (armed citizenry as political community) and the power/exit atoms (institutional for interpreters, organized for advocacy groups, institutional for health authorities) derive the right directional values automatically.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not appear to have suffered mandate death. The founding problem (risk of tyranny through standing armies) has not been definitively solved in the readings of its proponents. The civic republican narrative remains active in constitutional debate and in political theory. The reading is not maintained theatrically — it is defended through genuine scholarly and legal argument. The theater ratio (0.18) is low precisely because the reading is alive as an interpretive commitment, not as performative maintenance. There is no stage in the historical record where the mandate became obsolete but the constraint persisted; the reading has gained institutional force over time (especially post-2008 Supreme Court decisions), not lost it. The question of mandate obsolescence is instead captured in the founding_problem_status: is the founding problem still live? That is contested (not dead), so mandatrophy certification does not apply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_era_militia_composition,
    'What was the actual composition and organization of militias in the founding era? Were they universal armed citizenry or organized state institutions with professional officers?',
    'Historical analysis of state militia statutes, Revolutionary War records, and Founders'' correspondence on militia organization and participation. Competing scholarly interpretations (Carp, Rohman, Rakove, Foner) offer different reconstructions.',
    'If militia were primarily organized state institutions, the collective security reading gains evidentiary support and the universal armed citizenry framing appears as a post-hoc originalist construction. If militia were indeed universal armed citizenry, this reading''s historical claim is vindicated and the individual right reading''s grounding in personal self-defense appears less connected to founding intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_era_militia_composition, empirical, 'Whether founding-era militia were universal distributed armed citizenry or organized state institutions.').

omega_variable(
    civic_virtue_vs_individual_right_binary,
    'Is the distinction between civic virtue (political participation) and individual right (personal autonomy) logically exhaustive of the Founders'' intent, or is founding-era political thought better understood as integrating both dimensions without sharp separation?',
    'Philosophical and historical analysis of founding-era republicanism and liberalism: did the Founders compartmentalize these frameworks, or did they operate as integrated commitments? Examination of texts (The Federalist, state constitutions, founding speeches) for evidence of integrated vs. compartmentalized reasoning.',
    'If integrated, both this reading and the individual right reading are partially correct, and the sibling readings coexist more coherently than either individually claims. If compartmentalized, this reading''s framing of civic virtue as primary is empirically supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civic_virtue_vs_individual_right_binary, conceptual, 'Whether founding political thought separated civic virtue from individual rights or integrated them.').

omega_variable(
    suppression_of_alternative_readings,
    'Does the institutionalization of originalist interpretation in Supreme Court doctrine and legal academia suppress or marginalize alternative readings (collective security, living constitutionalism) such that the constraint''s persistence depends on suppression?',
    'Analysis of legal publication patterns, citations in major decisions, representation in law school curricula, and access to courts for alternative readings. Comparison of space given to originalist vs. alternative interpretive methods in high-impact venues.',
    'If originalism is institutionally privileged and alternatives are suppressed, extractiveness should increase (the reading persists through institutional power, not mere interpretive coherence). If alternative readings retain equal discursive space and institutional access, the low-suppression measurement is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_readings, empirical, 'Whether originalist institutional dominance suppresses competing constitutional readings.').

omega_variable(
    victim_status_of_excluded_voices,
    'Are public health authorities and violence-prevention advocates genuine victims of this reading, or are they better understood as parties with a competing reading who bear no extractive cost because no transfer is enforced against them?',
    'Analysis of regulatory authority: does this reading legally prevent public health authorities from regulating firearms, or does it merely constrain their options within a larger legal space? Measurement of actual authority foreclosed vs. authority remaining.',
    'If this reading legally prevents public health regulation, public health authorities are targets/victims. If this reading merely weights against regulation while preserving some authority, the victim framing is too strong, and the constraint should be reclassified as coexistent with (rather than extractive from) the public health frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_excluded_voices, empirical, 'Whether the originalist civic virtue reading legally forecloses public health authority or merely constrains its weight in constitutional balancing.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding problem (risk of tyranny through standing armies) been structurally solved by modern constitutional checks, professional military subordination to civilian control, and international security arrangements?',
    'Political theory analysis of whether modern constitutional and security structures adequately prevent tyranny without relying on distributed armed citizenry. Empirical assessment of actual risks of tyranny in constitutional democracies.',
    'If the founding problem is solved, the civic republican framing becomes mandatropic — it persists as a constraint but its coordinate function is obsolete. The reading would migrate from rope toward piton. If the founding problem remains live (risk of tyranny is real and distributed armed citizenry is a necessary check), the rope classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the founding problem (tyranny through standing armies) remains live in modern constitutional democracies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__originalist_civic_virtue_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t5, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement_basis(seco_tr_t5, observed).
narrative_ontology:measurement(seco_tr_t10, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(seco_tr_t10, observed).
narrative_ontology:measurement(seco_tr_t15, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 15, 0.16).
narrative_ontology:measurement_basis(seco_tr_t15, observed).
narrative_ontology:measurement(seco_tr_t20, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(seco_tr_t20, observed).
narrative_ontology:measurement(seco_tr_t25, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement_basis(seco_tr_t25, observed).
narrative_ontology:measurement(seco_tr_t30, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(seco_tr_t30, observed).
narrative_ontology:measurement(seco_tr_t35, second_amendment_text__originalist_civic_virtue_reading, theater_ratio, 35, 0.18).
narrative_ontology:measurement_basis(seco_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t5, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement_basis(seco_be_t5, observed).
narrative_ontology:measurement(seco_be_t10, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement_basis(seco_be_t10, observed).
narrative_ontology:measurement(seco_be_t15, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 15, 0.28).
narrative_ontology:measurement_basis(seco_be_t15, observed).
narrative_ontology:measurement(seco_be_t20, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement_basis(seco_be_t20, observed).
narrative_ontology:measurement(seco_be_t25, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(seco_be_t25, observed).
narrative_ontology:measurement(seco_be_t30, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 30, 0.31).
narrative_ontology:measurement_basis(seco_be_t30, observed).
narrative_ontology:measurement(seco_be_t35, second_amendment_text__originalist_civic_virtue_reading, base_extractiveness, 35, 0.31).
narrative_ontology:measurement_basis(seco_be_t35, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(second_amendment_text__originalist_civic_virtue_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__originalist_civic_virtue_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(second_amendment_text__originalist_civic_virtue_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__collective_security_reading).
narrative_ontology:affects_constraint(second_amendment_text__originalist_civic_virtue_reading, second_amendment_text__individual_right_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested Second Amendment kernel. The three sibling readings (originalist_civic_virtue_reading, collective_security_reading, individual_right_reading) are structurally distinct constraints because they instantiate different beneficiary sets, different founding problems, and different ε values. The originalist reading emphasizes political community and civic virtue (moderate extractiveness, genuine coordination function). The collective security reading emphasizes state regulatory authority (potentially higher extractiveness if the state uses regulation coercively). The individual right reading emphasizes personal autonomy and self-defense (different coordination function, different extractiveness profile). These are NOT three measurements of one constraint — they are three constraints sharing a kernel. Linked via affects_constraints for contamination propagation and constraint family analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
