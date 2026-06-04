% ============================================================================
% CONSTRAINT STORY: bill_of_rights_1791__security_arms_amendments
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bill_of_rights_1791__security_arms_amendments, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: bill_of_rights_1791__security_arms_amendments
 *   human_readable: Second and Third Amendments: Security Against Standing Army Through Militia and Home Inviolability
 *   domain: political/legal/constitutional
 *
 * SUMMARY:
 *   The Second and Third Amendments constitute one reading of the 1791 Bill
 *   of Rights kernel, specifically the founders' commitment to securing the
 *   people against the standing army they feared. This reading interprets
 *   these amendments as a unified constitutional defense mechanism: the
 *   militia clause and individual right to bear arms (Second Amendment)
 *   prevent the federal government from disarming the population, while the
 *   quartering prohibition (Third Amendment) prevents the military from
 *   establishing domestic garrison power over civilians. The reading holds
 *   that the founders understood a heavily armed citizenry and protected
 *   homes as structural checks on standing army dominance. This constraint
 *   exhibits structural properties of a Tangled Rope: it coordinates
 *   legitimate defense (beneficiaries include armed citizens and
 *   householders), it enforces asymmetric extraction (conscription burdens,
 *   disarmament of excluded populations), and it requires active
 *   constitutional enforcement and interpretation. The theater ratio has
 *   increased from 1791 (strict adherence to militia structure) to the
 *   present (elaborate jurisprudential debates over scope of regulation),
 *   reflecting both increased interpretive complexity and increased
 *   performative content in adjudication. The suppression requirement has
 *   risen as the constraint's enforcement has extended to more populations
 *   and more contexts, though the enforcement asymmetry has also deepened —
 *   historical exclusions of women, enslaved persons, Native Americans, and
 *   minorities from militia participation and bearing arms rights have been
 *   formalized into doctrine rather than treated as violative of the
 *   constraint.
 *
 * KEY AGENTS:
 *   - Armed Citizenry and Householders: Primary beneficiaries (institutional/arbitrage) — protected from military quartering and disarmament; enjoy constitutional right to bear arms for self-defense and militia service
 *   - Federal Military Authority: Primary constraint-bearer (organized/constrained) — prohibited from peacetime standing army dominance, forced quartering, and unilateral civilian disarmament; benefits from conscription and insurrection powers
 *   - Conscripted Militia Members: Secondary beneficiary and victim (moderate/constrained) — benefit from militia framework for collective defense; bear costs of conscription and training obligation
 *   - Disarmed and Excluded Populations: Primary victims (powerless/trapped) — bear full cost of selective militia exclusion (race, gender, property, literacy); cannot organize effective resistance; no constitutional protection of their security
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a historically contingent privilege structure as an inalienable natural right
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bill_of_rights_1791__security_arms_amendments, 0.38).
domain_priors:suppression_score(bill_of_rights_1791__security_arms_amendments, 0.52).
domain_priors:theater_ratio(bill_of_rights_1791__security_arms_amendments, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bill_of_rights_1791__security_arms_amendments, extractiveness, 0.38).
narrative_ontology:constraint_metric(bill_of_rights_1791__security_arms_amendments, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(bill_of_rights_1791__security_arms_amendments, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bill_of_rights_1791__security_arms_amendments, tangled_rope).
narrative_ontology:human_readable(bill_of_rights_1791__security_arms_amendments, "Second and Third Amendments: Security Against Standing Army Through Militia and Home Inviolability").
narrative_ontology:topic_domain(bill_of_rights_1791__security_arms_amendments, "political/legal/constitutional").

domain_priors:requires_active_enforcement(bill_of_rights_1791__security_arms_amendments).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(bill_of_rights_1791__security_arms_amendments, 'a51be214-6788-4b3a-8ed4-7466957c1640').
narrative_ontology:cs_kernel_codification('a51be214-6788-4b3a-8ed4-7466957c1640', formalized).
narrative_ontology:cs_authority_grounding('a51be214-6788-4b3a-8ed4-7466957c1640', lineage).
narrative_ontology:cs_interpretation_layer_present('a51be214-6788-4b3a-8ed4-7466957c1640').
narrative_ontology:cs_reading_relation('a51be214-6788-4b3a-8ed4-7466957c1640', bill_of_rights_1791__criminal_procedure_amendments, coexists_with).
narrative_ontology:cs_reading_relation('a51be214-6788-4b3a-8ed4-7466957c1640', bill_of_rights_1791__expression_conscience_amendments, coexists_with).
narrative_ontology:cs_reading_relation('a51be214-6788-4b3a-8ed4-7466957c1640', bill_of_rights_1791__reserved_powers_amendments, influences).
narrative_ontology:cs_axiom('a51be214-6788-4b3a-8ed4-7466957c1640', foundational, standing_army_domination_incompatible_with_republic).
narrative_ontology:cs_axiom_status(standing_army_domination_incompatible_with_republic, holdable).
narrative_ontology:cs_axiom_grounding('a51be214-6788-4b3a-8ed4-7466957c1640', standing_army_domination_incompatible_with_republic, deontological).
narrative_ontology:cs_axiom('a51be214-6788-4b3a-8ed4-7466957c1640', foundational, armed_citizenry_natural_security_check).
narrative_ontology:cs_axiom_status(armed_citizenry_natural_security_check, holdable).
narrative_ontology:cs_axiom_grounding('a51be214-6788-4b3a-8ed4-7466957c1640', armed_citizenry_natural_security_check, deontological).
narrative_ontology:cs_reference_frame('a51be214-6788-4b3a-8ed4-7466957c1640', militia_as_bulwark_against_tyranny).
narrative_ontology:cs_drift_state('a51be214-6788-4b3a-8ed4-7466957c1640', contemporary_professional_military_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a51be214-6788-4b3a-8ed4-7466957c1640', '').
narrative_ontology:cs_kernel_id(bill_of_rights_1791__security_arms_amendments, bill_of_rights_1791).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__security_arms_amendments, armed_citizenry).
narrative_ontology:constraint_beneficiary(bill_of_rights_1791__security_arms_amendments, property_owners_householders).
narrative_ontology:constraint_victim(bill_of_rights_1791__security_arms_amendments, federal_military_authority).
narrative_ontology:constraint_victim(bill_of_rights_1791__security_arms_amendments, conscripted_persons).
narrative_ontology:constraint_victim(bill_of_rights_1791__security_arms_amendments, disarmed_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISARMED POPULATION (SNARE) — Structurally barred from militia participation or home defense through literacy requirements, property qualifications, race/gender exclusions, and enforcement asymmetry. Bears the full cost of selective armament; cannot exit or organize effective resistance. Maximum extraction under the guise of constitutional protection.
constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSCRIPTED MILITIA MEMBER (TANGLED ROPE) — Benefits from constitutional militia framework (defense against invasion, suppression of insurrection); bears costs of conscription, training obligation, and liability for casualties. Exit options constrained by legal duty and social pressure. Mixed coordination and extraction.
constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ARMED PROPERTIED CITIZEN (ROPE) — Experiences the constraint as pure coordination: the Second Amendment protects the right to bear arms for lawful purposes (self-defense, militia service, hunting). The Third Amendment forbids peacetime quartering of troops in their homes. Net beneficiary with high exit options (can organize, advocate, transfer property). Extraction runs toward this agent as protection.
constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL MILITARY AUTHORITY (TANGLED ROPE) — Constrained by Second and Third Amendment prohibitions on standing army peacetime dominance and forced quartering; benefits from militia clauses (conscription, national defense coordination, insurrection suppression). Exit options constrained by constitutional language; agency limited but real (interpretation, enforcement, militia regulation). Mixed coordination and extraction.
constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the right to self-defense and home inviolability are treated as natural rights inherent to human beings, antecedent to any government. This perspective holds the constraint as an inalienable truth rather than a contingent political arrangement. However, structural data reveals this as a false summit: the constraint's beneficiaries and victims are historically specific (race, class, gender, property qualifications). The 'natural law' framing naturalizes a contingent privilege structure.
constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bill_of_rights_1791__security_arms_amendments_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bill_of_rights_1791__security_arms_amendments, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(bill_of_rights_1791__security_arms_amendments_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint coordinates legitimate defense (collective militia security) and protects individuals from military domination (Third Amendment quartering prohibition). However, it extractively privileges certain populations (armed property-owners, militia-eligible males) while imposing costs on others (conscription, disarmament of excluded groups, forfeiture of personal autonomy to militia duty). The baseline extractiveness reflects that the coordination benefit is real but the benefit distribution is asymmetric. The trajectory (0.22 → 0.38 over 235 years) reflects deepening exclusions: as female suffrage, integration, and broader citizenship have expanded nominally, militia participation and bearing-rights doctrines have become more formally stratified by jurisprudential narrowing (reasonable regulation, categorical exceptions for felons, domestic abusers, etc.), increasing the extraction burden on expanding populations while maintaining the ostensible universality of the right. Suppression (0.52): Moderate-high. Significant barriers exist to exercising the constraint's benefits: property qualifications (historical and de facto), literacy requirements, criminal records, age restrictions, geographic access to militia training and armories, de facto gender exclusion from militia command structures, racial exclusion from armed service and bearing-rights enjoyment, and enforcement asymmetry (quartering prohibition strictly enforced on federal military; disarmament strictly enforced on populations deemed threats). Theater ratio (0.48): Moderate. The constraint involves both genuine functional activity (militia training, home protection, constitutional litigation) and considerable performative content (elaborate jurisprudential debates about scope of regulation, symbolic rhetoric about founders' intent, theatrical compliance with exceptions while maintaining nominal universality of rights). The theater ratio has increased over time as the gap between nominal universality and actual exclusion has widened, requiring more elaborate doctrinal performance to maintain coherence.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence from a single set of structural data. The armed propertied citizen sees the constraint as pure coordination (Rope) — a legitimate framework for collective defense and home protection that imposes no extractive burden. The federal military authority sees mixed coordination and constraint (Tangled Rope) — they benefit from militia and insurrection-suppression powers but are constrained by quartering prohibition and disarmament prohibitions. The conscripted militia member sees mixed benefit and burden (Tangled Rope) — genuine defense coordination alongside conscription obligation. The disarmed population sees pure extraction (Snare) — they bear suppression and exclusion without benefit or exit. The civilizational analytical observer risks seeing natural law (Mountain) — an inalienable right to self-defense and home inviolability as an inherent feature of human society, not a contingent constitutional arrangement. The perspectival gap reveals that the constraint's coherence depends on which agent's experience is taken as the norm. If the armed propertied citizen's experience is normalized, the constraint appears as protection. If the disarmed population's experience is centered, it appears as oppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from the agent's structural position relative to the constraint's benefit and cost flows. The disarmed population (powerless/trapped) experiences maximum directionality toward the constraint (d ≈ 0.95) — they bear suppression and exclusion costs with no exit capacity. The armed propertied citizen (institutional/arbitrage) experiences minimum directionality (d ≈ 0.10) — they capture the constraint's benefits with high exit capacity (can organize politically, relocate, change citizenship). The conscripted militia member (moderate/constrained) experiences mid-range directionality (d ≈ 0.58) — they both benefit (collective defense) and bear costs (conscription obligation) with constrained exit. The federal military authority (organized/constrained) experiences slight beneficiary directionality (d ≈ 0.25) — they are constrained by the prohibition on standing army dominance, but they benefit from conscription and insurrection-suppression powers. The analytical observer (analytical/analytical) occupies a structural position that risks naturalizing the constraint (d ≈ 0.72), treating what is historically contingent as inherent. The engine's directionality derivation should reveal the false summit signature: the mountain classification at the analytical context naturalizes exclusionary practices as natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that all perspectives are structurally valid readings of the same constraint. The constraint is simultaneously a Rope (for beneficiaries), a Tangled Rope (for moderate agents and the military authority), a Snare (for excluded populations), and a false-summit Mountain (for observers who naturalize the exclusions). The mandatrophy is not 'which type is correct?' but 'which population's structural position are we measuring from?' The claim that the constraint 'secures the people against standing armies' is true for the armed propertied citizenry and false for the disarmed populations — the constraint secures some against armies while reinforcing their domination of others. The Tangled Rope classification at the claimed_type level represents the constraint's true structural character: it coordinates legitimate defense while extractively privileging certain populations and burdening others. The false summit at the analytical level (Mountain) is diagnostic: the framing 'natural right to self-defense' naturalizes what are historically contingent exclusions and privilege structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_clause_interpretation,
    'Does the militia clause (''A well regulated Militia, being necessary to the security of a free State'') condition the right to bear arms on militia service, or is it merely prefatory?',
    'Historical originalist analysis of founding texts, state constitutions, and ratification debates; linguistic analysis of prefatory vs operative clauses in 18th century legal documents; longitudinal jurisprudence tracking (DC v. Heller, McDonald v. Chicago)',
    'If conditioning: militia-only reading narrows beneficiaries to organized militia members; victim set changes to include civilians excluded from militia. If prefatory only: individual right reading broadens beneficiaries to all citizens; victim set remains conscripted and disarmed populations. Extractiveness shifts from 0.38 (balanced) toward 0.52 (higher extraction for non-militia-members).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(militia_clause_interpretation, conceptual, 'Scope of the militia clause: conditioning vs. prefatory function').

omega_variable(
    quartering_standing_army_entanglement,
    'Does the Third Amendment''s quartering prohibition imply a founders'' theory of standing armies as inherently extractive, or does it merely regulate one specific mechanism?',
    'Historical analysis of founders'' rhetoric on standing armies (Anti-Federalist Papers, state ratifying conventions, founding generation correspondence); comparison with founding-era European standing army doctrine; examination of whether quartering prohibitions alone would prevent military domination',
    'If inherently extractive (founders'' theory): standing armies per se are illegitimate; the constraint extends to peacetime military structure itself, not just quartering mechanics. Extractiveness interpretation shifts to treat the entire military hierarchy as structurally extractive. If merely mechanical regulation: the constraint is a narrow prohibition on one enforcement method; standing armies themselves are legitimate. Classification stabilizes at Tangled Rope. Axiom status on standing_army_domination becomes overridden or holdable depending on interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quartering_standing_army_entanglement, conceptual, 'Whether the founders rejected standing armies per se or only specific quartering mechanics').

omega_variable(
    beneficiary_exclusion_mechanisms,
    'What proportion of the constraint''s extractive burden falls on historically excluded populations (enslaved persons, women, non-property-owners, racial minorities) versus on the federal military authority?',
    'Historical demographic analysis: state-by-state militia enrollment rates by race, gender, property status, enslaved status; comparison of enforcement asymmetry (quartering prohibition strictly enforced on military vs. disarmament strictly enforced on disempowered groups); archival evidence of exclusion mechanisms (literacy tests, property qualifications, racial bars)',
    'If exclusion mechanisms account for > 60% of suppression: the constraint is primarily a mechanism for white property-owner protection against both the military and disempowered populations. Victim set shifts definitionally — the primary victims become the excluded populations, not the military authority. Snare classification for powerless agents confirmed at even higher confidence. Extractiveness may rise to 0.55+. If exclusion is incidental: the constraint structures balanced extraction between military and armed citizenry; extractiveness holds at 0.38.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_exclusion_mechanisms, empirical, 'Proportional burden of exclusion mechanisms on historically marginalized populations').

omega_variable(
    reading_kernel_contest,
    'Is this reading (security_arms_amendments) a coherent kernel reading, or does it conflate multiple distinct constitutional commitments (criminal procedure, expression/conscience, reserved powers)?',
    'Textual analysis: Do the Second and Third Amendments share a single foundational axiom, or do they operate from different authority groundings? Jurisprudential analysis: Do courts treat these amendments as a coherent bloc or as separate doctrinal domains? Historical analysis: Did the founders and ratifying states understand these amendments as unified defense against standing armies, or as separate protections?',
    'If conflated: the reading dissolves into a constraint family (three separate stories, one per amendment cluster or per structural purpose). If coherent: the reading stands as a unified security-focused interpretation of the Bill of Rights. Axiom structure in cs_structure.axioms will be adjusted accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Unity vs. decomposition of the security_arms_amendments reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bill_of_rights_1791__security_arms_amendments, 0, 235).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bill_tr_t0, bill_of_rights_1791__security_arms_amendments, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bill_tr_t100, bill_of_rights_1791__security_arms_amendments, theater_ratio, 100, 0.42).
narrative_ontology:measurement(bill_tr_t235, bill_of_rights_1791__security_arms_amendments, theater_ratio, 235, 0.48).

% Extraction over time
narrative_ontology:measurement(bill_be_t0, bill_of_rights_1791__security_arms_amendments, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(bill_be_t50, bill_of_rights_1791__security_arms_amendments, base_extractiveness, 50, 0.35).
narrative_ontology:measurement(bill_be_t235, bill_of_rights_1791__security_arms_amendments, base_extractiveness, 235, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(bill_su_t0, bill_of_rights_1791__security_arms_amendments, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(bill_su_t100, bill_of_rights_1791__security_arms_amendments, suppression_requirement, 100, 0.48).
narrative_ontology:measurement(bill_su_t235, bill_of_rights_1791__security_arms_amendments, suppression_requirement, 235, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bill_of_rights_1791__security_arms_amendments, enforcement_mechanism).
narrative_ontology:affects_constraint(bill_of_rights_1791__security_arms_amendments, bill_of_rights_1791__criminal_procedure_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__security_arms_amendments, bill_of_rights_1791__expression_conscience_amendments).
narrative_ontology:affects_constraint(bill_of_rights_1791__security_arms_amendments, bill_of_rights_1791__reserved_powers_amendments).

% DUAL FORMULATION NOTE:
% The security_arms_amendments reading is one of four structurally distinct constraint readings of the 1791 Bill of Rights kernel. Each reading interprets the kernel through a different foundational axiom and authority grounding. This reading emphasizes military-civilian power balance and treats the founders' anti-standing-army commitment as the organizing principle. The criminal_procedure reading emphasizes procedural rights in criminal adjudication; the expression_conscience reading emphasizes ideational autonomy; the reserved_powers reading emphasizes federalism. Each reading generates a separate constraint story with its own beneficiary/victim structure, extractiveness, and perspectives. The network links represent structural influence: judicial interpretation of criminal procedure shapes the contours of armed self-defense rights (e.g., categorical prohibition on felons bearing arms); expression/conscience protections shape the symbolic resonance of militia rights rhetoric; reserved powers doctrine constrains federal power to regulate state militias. All four readings coexist in contemporary constitutional discourse, held by different judicial and political coalitions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
