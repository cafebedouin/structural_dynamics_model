% ============================================================================
% CONSTRAINT STORY: us_constitution__original_constitution_1787
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution__original_constitution_1787, []).

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
 *   constraint_id: us_constitution__original_constitution_1787
 *   human_readable: The Original 1787 Constitution as a Machine of Enumerated Powers
 *   domain: political/legal/constitutional_authority
 *
 * SUMMARY:
 *   This constraint instantiates one specific reading of the contested kernel
 *   'us_constitution': the claim that the unamended 1787 text is a 'complete
 *   machine of enumerated powers' whose structural design—not its later
 *   amendments—does the real constitutional work. This reading asserts that
 *   the Constitution's authority and operative structure derive from the
 *   founding text's architectural choices (separation of powers, federalism,
 *   property protections, slavery entrenchment), not from later grafted
 *   amendments (Bill of Rights 1791, post-Civil War amendments, Progressive
 *   Era amendments, etc.). The reading's core beneficiary is the propertied
 *   founding coalition whose interests the 1787 structure encodes and
 *   protects. The core victims are the enslaved (who are structurally counted
 *   as 3/5 for representation purposes while denied legal personhood),
 *   non-property holders (systematically disenfranchised), and unrepresented
 *   populations (women, Native Americans, foreigners). The constraint
 *   exhibits mixed coordination and extraction: it coordinates the interests
 *   of the founding coalition through checks and balances, federalism, and
 *   enumerated powers, while simultaneously extracting from those excluded
 *   from political participation and property protection. The theater ratio
 *   (0.45) reflects that the original text's enforcement is substantially
 *   direct—the suppression mechanisms (disenfranchisement, slavery, property
 *   qualifications) are textually explicit rather than performative—though
 *   later originalist invocations of the text's 'purity' do introduce
 *   theater. The extractiveness measurement trajectory shows gradual decline
 *   from 0.58 to 0.35 across the century (1787–1887), reflecting the
 *   cumulative effect of amendments expanding the franchise and introducing
 *   post-structure protections, even as the underlying 1787 framework
 *   persists. The suppression measurement trajectory shows initial increase
 *   (1787–1817, from 0.62 to 0.64) as the gap between constitutional text and
 *   excluded populations widened, then decline (1817–1887) as amendments
 *   began to address the gap.
 *
 * KEY AGENTS:
 *   - Propertied Founding Coalition: Primary beneficiary (institutional/arbitrage) — delegates whose economic interests the 1787 structure protects; experience it as pure coordination with no extraction cost
 *   - Enslaved Persons: Primary victim (powerless/trapped) — structurally incorporated as 3/5 of a person for representation and taxation purposes while denied legal personhood; experience maximum extraction with no exit
 *   - Non-Property-Holding Free Persons: Secondary victim (moderate/constrained) — excluded from political participation by property and tax qualifications; constrained but not completely trapped; can acquire property to gain voice
 *   - Unrepresented Populations (women, Native Americans): Tertiary victim (powerless/identity_locked) — structurally absent from the founding coalition's calculus; identity as non-citizens/non-persons fused with constitutional structure; psychological exit from incorporation itself is bound up with identity
 *   - Advocates for Constitutional Revision: Organized agents (organized/constrained) — abolitionists, suffragists, reformers who perceive amendment as the pathway to transformation; constrained by the difficulty of amending but empowered by Article V's existence
 *   - Originalist Legal Tradition: Institutional actor (institutional/arbitrage) — invokes the 1787 text as foundational, performing a return to textual purity while executing interpretive work; maintains the constraint through legitimacy-seeking
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the 1787 choices as immutable structural necessities rather than as historically contingent design decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution__original_constitution_1787, 0.58).
domain_priors:suppression_score(us_constitution__original_constitution_1787, 0.62).
domain_priors:theater_ratio(us_constitution__original_constitution_1787, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution__original_constitution_1787, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_constitution__original_constitution_1787, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(us_constitution__original_constitution_1787, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution__original_constitution_1787, tangled_rope).
narrative_ontology:human_readable(us_constitution__original_constitution_1787, "The Original 1787 Constitution as a Machine of Enumerated Powers").
narrative_ontology:topic_domain(us_constitution__original_constitution_1787, "political/legal/constitutional_authority").

domain_priors:requires_active_enforcement(us_constitution__original_constitution_1787).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution__original_constitution_1787, '2748598d-cd5b-43e9-ab86-87d5f6a0b3fc').
narrative_ontology:cs_kernel_codification('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', formalized).
narrative_ontology:cs_authority_grounding('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', lineage).
narrative_ontology:cs_interpretation_layer_present('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc').
narrative_ontology:cs_reading_relation('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', us_constitution__bill_of_rights_1791, forecloses).
narrative_ontology:cs_reading_relation('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', us_constitution__failed_amendments, coexists_with).
narrative_ontology:cs_reading_relation('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', us_constitution__later_amendment_eras, coexists_with).
narrative_ontology:cs_reading_relation('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', us_constitution__pre_constitutional_frameworks, influences).
narrative_ontology:cs_axiom('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', foundational, unamended_1787_text_is_complete).
narrative_ontology:cs_axiom_status(unamended_1787_text_is_complete, holdable).
narrative_ontology:cs_axiom_grounding('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', unamended_1787_text_is_complete, conventional).
narrative_ontology:cs_axiom('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', foundational, structural_design_precedes_rights_additions).
narrative_ontology:cs_axiom_status(structural_design_precedes_rights_additions, holdable).
narrative_ontology:cs_axiom_grounding('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', structural_design_precedes_rights_additions, empirically_contingent).
narrative_ontology:cs_reference_frame('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', enumerated_powers_machine_1787).
narrative_ontology:cs_drift_state('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', contemporary_post_civil_rights, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2748598d-cd5b-43e9-ab86-87d5f6a0b3fc', '').
narrative_ontology:cs_kernel_id(us_constitution__original_constitution_1787, us_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution__original_constitution_1787, propertied_founding_coalition).
narrative_ontology:constraint_beneficiary(us_constitution__original_constitution_1787, slave_holding_states).
narrative_ontology:constraint_victim(us_constitution__original_constitution_1787, enslaved_persons).
narrative_ontology:constraint_victim(us_constitution__original_constitution_1787, non_property_holders).
narrative_ontology:constraint_victim(us_constitution__original_constitution_1787, unrepresented_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ENSLAVED AND NON-PROPERTY HOLDERS (SNARE) — Trapped within the constitutional structure with no exit. The 3/5 compromise, slavery's constitutional entrenchment, and property qualifications for suffrage encode extraction into the foundational text itself. No alternatives presented; the constitutional machine is the only political framework available. Maximum experienced extraction with full suppression of exit options.
constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PROPERTIED FOUNDING COALITION (ROPE) — Experiences the enumerated powers structure as pure coordination of their shared interests. The separation of powers, federalism, and property protections solve the collective action problem of establishing a stable commercial republic without central tyranny. Net beneficiary with maximum agency and exit optionality—can ratify or refuse. The constitutional design serves their coordination function with minimal experienced extraction.
constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: NON-SLAVEHOLDING FREE PERSONS WITHOUT PROPERTY (TANGLED ROPE) — Structurally constrained by property qualifications and exclusion from political participation, yet also benefit from the constitutional order's protection of commerce, contract, and mobility (compared to feudal or monarchical alternatives). Experience both coordination benefits (commercial stability, rule of law) and asymmetric extraction (disenfranchisement, limited voice). High suppression but not absolute—some pathways to property acquisition exist, albeit with barriers.
constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVOCATES FOR CONSTITUTIONAL REVISION (SCAFFOLD) — Organized groups (abolitionists, suffragists, reformers) perceive the original 1787 structure as a temporary coordination with a built-in sunset: the amendment mechanism itself is the pathway to permanent revision. The constraint has low effectiveness from this perspective because it carries its own negation—a functioning amendment procedure that allows structural transformation. Theater remains low because the original text's enforcement is direct, not performative; the sunset is real and proximate.
constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ORIGINALIST INVOCATION OF 1787 PURITY (PITON) — In contemporary jurisprudence, appeals to 'the original 1787 text as complete' function as a performative claim that masks subsequent institutional sedimentation. The idea that one can recover the 'pure' enumerated-powers machine stripped of 200 years of amendment and practice is largely theatrical—the text is always read through interpretive traditions, case law, and structural evolution. Theater ratio is high from this perspective: originalism presents itself as a return to textual purity while actually executing sophisticated reinterpretation. This is a piton—the invocation persists through institutional inertia and legitimacy-seeking, not because the claim is functionally coherent.
constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical stance, the structural tension between enumerated federal powers and reserved state sovereignty is presented as an immutable principle of federal design—a natural law of constitutional mechanics that cannot be changed without destroying the system itself. This perspective risks naturalizing what is historically contingent: the specific enumeration of powers, the federal-state split, the property protections, and the slavery compromises are all designed choices, not physical limits. However, the structural data contradicts the mountain classification—the engine will compute this as a false summit, revealing that 'federalism is immutable' naturalizes what are actually contestable institutional arrangements.
constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution__original_constitution_1787_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_constitution__original_constitution_1787, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution__original_constitution_1787, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_constitution__original_constitution_1787, TR),
    TR >= 0.70.

:- end_tests(us_constitution__original_constitution_1787_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The original 1787 structure entrenches extraction through multiple mechanisms: slavery (3/5 compromise + slave trade protection), property qualifications for voting, and the absence of rights guarantees. However, the extraction is not maximal (would be 0.75+) because the structure also provides genuine coordination benefits (stable commercial republic, rule of law, protection against centralized tyranny) that benefit even some constrained parties. The 0.58 value reflects that beneficiaries experience pure coordination (low extraction) while victims experience high extraction, and the average across perspectives is moderate-high. Suppression (0.62): High. Structural suppression includes disenfranchisement by property/gender/race, slavery's absolute suppression of legal personhood, and the absence of positive rights guarantees. The suppression is not maximal because some exit pathways exist (property acquisition, migration within the union) and because the enumerated federal powers structure does limit centralized coercive capacity. Theater ratio (0.45): Moderate-low. The suppression mechanisms are substantially explicit and enforced directly (e.g., slave patrols, property courts), not performative. The theater emerges primarily in later originalist invocations of the text's 'purity' and coherence, which retroactively impose coherence on what was a negotiated compromise document. The measurements show extractiveness declining as amendments expand the franchise (13th, 14th, 15th, 19th, 24th) and suppression declining correspondingly, though the underlying 1787 structure (federalism, separation of powers) persists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The propertied founding coalition sees Rope—pure coordination of their shared interests in a stable commercial republic—with no extraction cost from their perspective. The enslaved and disenfranchised see Snare—maximum extraction with no alternatives. Non-property-holding free persons see Tangled Rope—genuine coordination benefits (rule of law, commerce protection) alongside asymmetric extraction (disenfranchisement, limited voice). Organized advocates for revision see Scaffold—a temporary structure with a built-in amendment pathway to transformation. Later originalists see Piton—the persistent invocation of the 1787 text as foundational becomes increasingly performative as reinterpretation layers accumulate. The civilizational analytical observer risks seeing Mountain—treating the founding choices as immutable principles of federal design—but the structural data reveals this as a false summit: the choices are historically contingent (slavery could have been prohibited, property qualifications could have been absent, direct democracy or pure federalism could have been chosen instead).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by the agent's structural position and relationship to the extraction flow. The propertied coalition as beneficiaries with arbitrage options experience d ≈ 0.05 (full beneficiary), producing negative f(d) and minimal experienced χ. The enslaved as victims with trapped exit experience d ≈ 0.95 (full target), producing maximum f(d) ≈ 1.42 and maximum experienced χ. Non-property-holding free persons experience d ≈ 0.65–0.75 (substantial target with some constrained agency), producing high f(d) ≈ 1.0–1.15. The organized advocates experience d ≈ 0.55–0.60 (mixed target/beneficiary status with constrained exit), producing moderate f(d) ≈ 0.75–0.85. The originalist institutional actor experiences d ≈ 0.20–0.30 (beneficiary of the text's legitimacy appeal with arbitrage to other interpretive traditions), producing low f(d) ≈ 0.02–0.30. The analytical observer experiences d ≈ 0.72 (canonical analytical), producing f(d) ≈ 1.15. These directionality values are implicit in the derived chi calculations; no overrides are necessary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_completeness_claim,
    'Is the 1787 text genuinely a ''complete machine'' of enumerated powers, or is this claim a later originalist reconstruction that retroactively imposes coherence on a compromise document?',
    'Historical analysis of founding-era discussions, debates, and amendments proposed during ratification; comparison of stated intent vs. textual gaps and ambiguities (e.g., commerce clause scope, necessary and proper clause generality)',
    'If truly complete: the 1787 structure is sufficient as a standalone constitutional order, and amendments are additions rather than corrections. If reconstructed: the ''complete machine'' framing is a reading imposed by later interpreters, and subsequent amendments represent belated remedies rather than perfections.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_completeness_claim, empirical, 'Whether the 1787 text is inherently complete or a later reconstruction').

omega_variable(
    suppression_mechanism_locus,
    'Is the constitutional suppression primarily structural (ambition counteracting ambition, as Madison framed it) or primarily extractive (property-protection, slavery-entrenchment, disenfranchisement)?',
    'Textual analysis of enumerated powers vs. reserved powers vs. unenumerated restrictions; historical analysis of intent behind specific provisions (commerce clause, slave trade clause, property takings); comparative analysis of how different constituencies experienced the 1787 framework',
    'If structural suppression dominates: the constraint is closer to Rope (coordination with built-in checks) from multiple perspectives. If extractive suppression dominates: the constraint is closer to Snare from the powerless perspective. This drives the classification gap between beneficiaries and victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_locus, empirical, 'Locus of suppression: structural balance vs. extractive entrenchment').

omega_variable(
    amendment_mechanism_status,
    'Does the Amendment V mechanism (Article V) function as a genuine exit route from the original constraint or as a performative outlet that preserves the foundational structure?',
    'Historical analysis of amendment success vs. failure rates; examination of which amendments succeeded and which failed; assessment of whether successful amendments altered core structural features or merely adjusted peripherals',
    'If genuine exit: the original 1787 structure is temporary (Scaffold perspective correct). If performative: most amendments are absorbed into the original structure without fundamentally altering it (Piton perspective more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_mechanism_status, empirical, 'Amendment mechanism as genuine exit vs. performative outlet').

omega_variable(
    three_fifths_compromise_extractiveness,
    'What proportion of the original constitution''s extractiveness derives specifically from the 3/5 compromise and slavery entrenchment vs. broader property-protection and disenfranchisement mechanisms?',
    'Structural decomposition: calculate extractiveness with and without slavery provisions; historical analysis of slave-state voting power in federal elections and legislative chambers; assessment of counterfactual constitutional order without slavery protection',
    'If slavery entrenchment accounts for >60% of total extractiveness: removing it would fundamentally alter the constraint type (possibly to Rope or lower Tangled Rope). If <40%: the constraint''s extractiveness persists independent of slavery, embedded in property and suffrage architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(three_fifths_compromise_extractiveness, empirical, 'Extractiveness attribution: slavery entrenchment vs. structural mechanisms').

omega_variable(
    reading_contest_underspecification,
    'Which sibling reading (bill_of_rights_1791, failed_amendments, later_amendment_eras, pre_constitutional_frameworks) most directly forecloses or coexists with the original_constitution_1787 reading?',
    'Logical analysis of each sibling''s core claim: does it accept or reject the premise that the 1787 text is a ''complete machine''? Does acceptance of the sibling reading require rejection of this reading''s axiomatic claim?',
    'If multiple readings coexist: the kernel is genuinely contested with no single winning reading (supports coexists_with relations). If one reading forecloses another: the contest resolves through either empirical evidence or normative commitment (supports forecloses relation). This determines the engine''s drift analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_underspecification, conceptual, 'Logical structure of kernel contest among the five readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution__original_constitution_1787, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(uscon1787_extract_1787, us_constitution__original_constitution_1787, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(uscon1787_extract_1817, us_constitution__original_constitution_1787, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(uscon1787_extract_1862, us_constitution__original_constitution_1787, base_extractiveness, 75, 0.48).
narrative_ontology:measurement(uscon1787_extract_1887, us_constitution__original_constitution_1787, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(uscon1787_suppress_1787, us_constitution__original_constitution_1787, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(uscon1787_suppress_1817, us_constitution__original_constitution_1787, suppression_requirement, 30, 0.64).
narrative_ontology:measurement(uscon1787_suppress_1862, us_constitution__original_constitution_1787, suppression_requirement, 75, 0.59).
narrative_ontology:measurement(uscon1787_suppress_1887, us_constitution__original_constitution_1787, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution__original_constitution_1787, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution__original_constitution_1787, us_constitution__bill_of_rights_1791).
narrative_ontology:affects_constraint(us_constitution__original_constitution_1787, us_constitution__failed_amendments).
narrative_ontology:affects_constraint(us_constitution__original_constitution_1787, us_constitution__later_amendment_eras).
narrative_ontology:affects_constraint(us_constitution__original_constitution_1787, us_constitution__pre_constitutional_frameworks).
narrative_ontology:affects_constraint(us_constitution__original_constitution_1787, slavery_entrenchment_constitutional_protection).
narrative_ontology:affects_constraint(us_constitution__original_constitution_1787, american_property_regime_1787_onward).

% DUAL FORMULATION NOTE:
% The original 1787 Constitution is best understood as one constraint in a constraint family spanning multiple kernel readings. This story models the claim that the unamused 1787 text is 'complete' and does the 'real work.' Sibling stories model the claims that the Bill of Rights completed it, that amendments define it, that the Articles baseline reframes it, and that rejected amendments constrain it. These are not alternative measurements of one constraint but distinct logical claims about which textual and institutional elements ground the Constitution's authority. The extractiveness values differ across readings because they attribute different victim sets and beneficiary structures: the 1787 reading identifies the slave-holding states and propertied coalition as beneficiaries; the Reconstruction reading shifts to identify freed persons and Republican Congress as reformed beneficiaries and ex-slaveholders as newly constrained. Separate the stories by reading and link via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
