% ============================================================================
% CONSTRAINT STORY: second_amendment_text__individual_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__individual_rights_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: second_amendment_text__individual_rights_reading
 *   human_readable: Second Amendment Individual Rights Reading
 *   domain: constitutional_law/firearms_policy/political_philosophy
 *
 * SUMMARY:
 *   The Second Amendment individual rights reading is a constitutional
 *   interpretation established in District of Columbia v. Heller (2008) that
 *   frames the right to bear arms as an individual right independent of
 *   militia service. This constraint story models ONE READING of the
 *   contested Second Amendment kernel. The individual rights reading
 *   instantiates a tangled rope structure: it provides genuine coordination
 *   function (enables lawful firearms ownership and self-defense), yet
 *   simultaneously creates asymmetric extraction through proliferation
 *   externalities that fall disproportionately on communities not controlling
 *   the reading. The constraint exhibits the full range of DR classification
 *   depending on observer position: beneficiaries see pure coordination
 *   (Rope), organizers see a temporary problem with sunset potential
 *   (Scaffold), institutions perform deference while maintaining
 *   contradictory logics (Piton), victims experience pure extraction (Snare),
 *   and moderate communities experience mixed coordination-extraction
 *   (Tangled Rope). The analytical observer risks naturalizing this reading
 *   as a consequence of universal natural rights philosophy (Mountain
 *   classification), but structural evidence reveals it as a contested
 *   21st-century interpretive innovation with identifiable beneficiaries and
 *   victims.
 *
 * KEY AGENTS:
 *   - Individual Gun Owners: Primary beneficiary (powerful/mobile) — access firearms with constitutional protection; net positive extraction flow from this reading
 *   - Firearms Manufacturers & Industry: Secondary beneficiary (institutional/arbitrage) — expanded market protected from regulation; significant profit extraction
 *   - Conservative Legal Scholars: Institutional beneficiary (institutional/constrained) — ideological investment and professional identity tied to originalist doctrine; constrained by need to maintain doctrinal coherence
 *   - Urban Violence Communities: Moderate victim (moderate/constrained) — experience genuine self-defense coordination need but bear disproportionate system-level harm from proliferation; constrained by poverty and enforcement gaps
 *   - Domestic Violence Victims: Primary victim (powerless/trapped) — abusers access firearms with minimal barriers; intimate-partner femicide risk elevated by constitutional protection of individual ownership; 922(g)(8) exception is narrow and poorly enforced
 *   - Mass Shooting Survivors & Families: Secondary victim (powerless/trapped) — bear concentrated extraction (grievous bodily harm, death, psychological trauma) as externality of proliferation enabled by this reading
 *   - Lower Federal Judiciary: Institutional actor (institutional/arbitrage) — performs deference to Heller while maintaining pre-Heller regulatory status quo; maintains contradictory logics to avoid Supreme Court collision
 *   - Gun Safety Regulation Coalition: Organized agent (organized/mobile) — sees reading as temporary institutional state with 20-30 year sunset via cultural/political shift; active exit path
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent interpretation as immutable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__individual_rights_reading, 0.58).
domain_priors:suppression_score(second_amendment_text__individual_rights_reading, 0.62).
domain_priors:theater_ratio(second_amendment_text__individual_rights_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__individual_rights_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(second_amendment_text__individual_rights_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(second_amendment_text__individual_rights_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__individual_rights_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__individual_rights_reading, "Second Amendment Individual Rights Reading").
narrative_ontology:topic_domain(second_amendment_text__individual_rights_reading, "constitutional_law/firearms_policy/political_philosophy").

domain_priors:requires_active_enforcement(second_amendment_text__individual_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__individual_rights_reading, '8118b73b-3667-445e-b02b-dae194be752a').
narrative_ontology:cs_kernel_codification('8118b73b-3667-445e-b02b-dae194be752a', fixed_text).
narrative_ontology:cs_authority_grounding('8118b73b-3667-445e-b02b-dae194be752a', lineage).
narrative_ontology:cs_interpretation_layer_present('8118b73b-3667-445e-b02b-dae194be752a').
narrative_ontology:cs_reading_relation('8118b73b-3667-445e-b02b-dae194be752a', second_amendment_text__collective_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('8118b73b-3667-445e-b02b-dae194be752a', second_amendment_text__hybrid_civic_reading, coexists_with).
narrative_ontology:cs_axiom('8118b73b-3667-445e-b02b-dae194be752a', foundational, individual_preexists_militia).
narrative_ontology:cs_axiom_status(individual_preexists_militia, holdable).
narrative_ontology:cs_axiom_grounding('8118b73b-3667-445e-b02b-dae194be752a', individual_preexists_militia, deontological).
narrative_ontology:cs_axiom('8118b73b-3667-445e-b02b-dae194be752a', secondary, constitutional_regulation_skepticism).
narrative_ontology:cs_axiom_status(constitutional_regulation_skepticism, holdable).
narrative_ontology:cs_axiom_grounding('8118b73b-3667-445e-b02b-dae194be752a', constitutional_regulation_skepticism, instrumental).
narrative_ontology:cs_reference_frame('8118b73b-3667-445e-b02b-dae194be752a', natural_law_self_defense).
narrative_ontology:cs_drift_state('8118b73b-3667-445e-b02b-dae194be752a', contemporary_post_heller, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8118b73b-3667-445e-b02b-dae194be752a', '').
narrative_ontology:cs_kernel_id(second_amendment_text__individual_rights_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_rights_reading, individual_gun_owners).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_rights_reading, firearms_manufacturers).
narrative_ontology:constraint_beneficiary(second_amendment_text__individual_rights_reading, conservative_legal_scholars).
narrative_ontology:constraint_victim(second_amendment_text__individual_rights_reading, urban_violence_communities).
narrative_ontology:constraint_victim(second_amendment_text__individual_rights_reading, domestic_violence_victims).
narrative_ontology:constraint_victim(second_amendment_text__individual_rights_reading, mass_shooting_survivors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL GUN OWNER — Powerful agent with genuine mobility and arbitrage (can acquire firearms under this reading; exit costs are low relative to benefits). Experiences the constraint as pure coordination: the Second Amendment guarantees their right and creates a predictable legal pathway for ownership. No perceived extraction — the constitutional protection aligns with their interests. This is the reading's canonical beneficiary.
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN VIOLENCE COMMUNITIES (TANGLED ROPE) — Moderate power, constrained exit. These communities experience genuine coordination function: firearms have legitimate self-defense roles, and the reading protects individual agency in self-protection. But they also bear disproportionate systemic extraction: proliferation of firearms without commensurate regulation increases ambient violence risk, mortality burden falls on these communities despite not controlling the constitutional reading. Mixed structure: some coordination benefit (access to self-defense) alongside substantial asymmetric extraction (system-level violence risk).
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMESTIC VIOLENCE VICTIMS (SNARE) — Powerless agents trapped in intimate abuse contexts. The individual rights reading creates structural extraction: abusers access firearms with minimal barriers (constitutional protection of 'individual' ownership applies regardless of intent to harm partners). Victims have no exit from the reading itself — Supreme Court jurisprudence has created a narrow exception (18 U.S.C. § 922(g)(8) — abusers subject to protective orders), but the reading's core logic prioritizes individual ownership over intimate-partner violence prevention. Maximum suppression: victims lack meaningful agency or alternative paths. This is the reading's canonical victim set.
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: GUN SAFETY REGULATION COALITION (SCAFFOLD) — Organized agents (Everytown, Brady Campaign, survivor groups) see the individual rights reading as a temporary institutional state with a built-in sunset mechanism: if the factual premises change (mass shooting incidence, empirical evidence on harm), the reading's justificatory foundation erodes. This perspective treats the reading as genuinely having an exit path via cultural shift and evidence accumulation — the theatrical performance of 'constitutional protection for all individuals' becomes untenable when individualism's harm consequences become undeniable. Low theater because this perspective acknowledges its own alternatives clearly; has sunset because organized agents see a 20-30 year horizon for cultural rebalancing. Exit path is available: reframe from 'individual rights' to 'rights with harm-responsibility' through amendment or interpretation shift.
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LOWER FEDERAL JUDICIARY (PITON) — Institutional actor that has nominally embraced the individual rights reading since DC v. Heller (2008) but continues to uphold many regulations without clear principled distinction between permissible and impermissible restrictions. Theater is high: courts perform deference to the Second Amendment while maintaining the regulatory status quo. The reading has atrophied from its original function (constraining gun regulation) into theatrical compliance with a Supreme Court mandate that the courts themselves actively work around through narrow tailoring, intermediate scrutiny, and historical tradition tests. The judges understand this as institutional inertia — they are maintaining two contradictory logics (strong individual right + strong regulatory power) to avoid collision with either the Supreme Court or political constituencies.
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL RIGHTS VIEW (MOUNTAIN) — From a civilizational/universal view, the individual rights reading grounds itself in natural law: individuals have a pre-political right to self-defense and to the means thereof; the Second Amendment merely recognizes this natural right rather than creating it. This perspective sees the reading as an immutable consequence of human nature and property rights philosophy, not a contingent institutional arrangement. However, the structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of what is actually a contested reading of a ambiguous text grounded in late-20th-century jurisprudence, not in pre-political natural law.
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: CONSERVATIVE LEGAL ESTABLISHMENT (TANGLED ROPE) — Institutional beneficiary that receives both genuine coordination benefit (the reading provides stable, predictable doctrine for conservative jurisprudence) and net extraction advantage (the reading enables alliance-building, donor mobilization, and cultural dominance in constitutionalism). Constrained rather than arbitrary exit because reversing the reading would require acknowledging interpretive error and political motivation — reputational cost is high. Theater moderate: genuine doctrinal and cultural investment in the reading.
constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__individual_rights_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(second_amendment_text__individual_rights_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__individual_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(second_amendment_text__individual_rights_reading, TR),
    TR >= 0.70.

:- end_tests(second_amendment_text__individual_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The individual rights reading enables substantial extraction through proliferation externalities. Beneficiaries (individual owners, manufacturers) capture direct benefits (access, profit) while costs (violence mortality, emergency services burden, psychological harm) are distributed to non-consenting communities. The extraction is not maximal (snare-level) because the reading also genuinely coordinates self-defense capacity and aligns with plausible constitutional values. The moderate-high value reflects: (1) proliferation externality burden ~25-35% attributable to regulatory permission granted by this reading, (2) concentrated victim burden (intimate partners, urban youth), (3) the reading's ideological entrenchment making cost-reduction via regulation politically difficult. Suppression (0.62): Moderate-high. The reading suppresses alternatives through: (1) constitutional entrenchment (Supreme Court authority makes reinterpretation procedurally difficult), (2) political mobilization (gun owners are more organized than gun violence prevention advocates), (3) legal barriers to regulation (strict scrutiny framework makes new regulations vulnerable to invalidation), (4) interpretive closure (the reading is treated as settled law rather than contestable). Victims have structural barriers to exit or reframing. Theater ratio (0.68): High. The lower judiciary performs deference to the individual rights doctrine while maintaining most pre-Heller regulations through narrow tailoring and intermediate scrutiny gymnastics. The reading's 'protective' function is partly theatrical — courts affirm regulations without coherent doctrinal grounding, treating the reading as performatively authoritative rather than substantively constraining. The theater has increased over the measurement interval (2014-2024) as post-Heller litigation has revealed doctrinal incoherence without changing outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This reading demonstrates maximum perspectival divergence. Individual gun owners see Rope (pure coordination — the reading guarantees their right and solves the coordination problem of lawful access). The gun safety coalition sees Scaffold (temporary institutional state with sunset pathway via cultural shift and evidence accumulation). Lower courts see Piton (performative maintenance of contradictory logics). Domestic violence victims see Snare (pure extraction with no exit). Urban communities see Tangled Rope (genuine self-defense coordination mixed with disproportionate system-level harm). Conservative legal establishment sees Tangled Rope (genuine doctrinal coordination alongside institutional extraction benefit). The analytical observer risks Mountain (naturalizing as consequence of universal natural rights). The perspectival gap reveals that the reading is not a neutral constitutional fact but a contested institutional arrangement with redistributive consequences.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status and exit options. Individual gun owners (beneficiary + mobile) have low d, experiencing negative chi (the constraint subsidizes them). Domestic violence victims (victim + trapped) have high d (~0.95), experiencing maximum chi (extraction runs toward them from the system). Urban communities (victim + constrained) have elevated d (~0.80-0.85), experiencing high chi but with some agency through exit costs. Conservative legal establishment (beneficiary/beneficiary-adjacent + constrained) has intermediate d, experiencing moderate positive extraction (constrained exit because doctrinal reversal carries reputational cost). Lower judiciary (neither pure beneficiary nor victim; institutional handler) derives d from their structural role as constraint implementers constrained by institutional hierarchy (~0.60-0.65). The gun safety coalition (organized/mobile, nominally victim-aligned but with institutional agency) derives d around 0.50, experiencing symmetric structural pressure. The analytical observer (neutral context) has canonical d for analytical power (~0.72). Directionality computation follows the sigmoid f(d), producing chi values that show: beneficiaries experience low/negative effective extraction; trapped victims experience maximum; moderate agents experience intermediate extraction; institutional coordinators experience moderate extraction despite nominal beneficiary status due to constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by instantiating a genuine tangled rope: the reading provides real coordination function (enables lawful self-defense firearms access; creates stable predictable doctrine) AND exhibits asymmetric extraction (proliferation externalizes violence risk to non-consenting communities, particularly intimate partners and urban youth). The coordination function is non-trivial — individuals do have legitimate self-defense interests, and the reading addresses real coordination demands around firearms ownership. The extraction is also non-trivial — the reading simultaneously enables a proliferation-driven mortality burden that falls on identifiable powerless groups. This is not confusion between two constraint types; it is a single constraint that does both: coordinates individual self-defense AND extracts from intimate partners and urban communities. The mandatrophy dissolves when we recognize that constitutional readings can genuinely coordinate on one dimension while extracting on another, and that net social valence is not determined by the presence of both functions but by the magnitude and distribution of each. This reading's net effect is extraction-dominant for society overall (empirical harm exceeds coordination benefit in population-weighted terms), but it remains tangled rope rather than pure snare because the coordination function is genuine and non-negligible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the Second Amendment''s individual rights reading a legitimate interpretation of the text, or does it require selective reading of the militia clause?',
    'Linguistic and historical analysis: does ''A well regulated Militia, being necessary to the security of a free State, the right of the people to keep and bear Arms, shall not be infringed'' grammatically permit an individual-rights-independent reading, or is militia service the enabling condition for the protected right?',
    'If individual rights are genuinely textually supported: the reading is legitimate constitutional doctrine. If the militia clause is a genuine condition: the reading is an interpretive innovation (judicial creation), not a discovery of pre-existing law.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Textual support for individual rights reading independent of militia clause').

omega_variable(
    harm_asymmetry_measurement,
    'How much of the mortality and injury burden in urban communities is attributable to the proliferation enabled by the individual rights reading vs. to enforcement gaps and socioeconomic factors?',
    'Comparative analysis: jurisdictions with equivalent individual rights protections but different enforcement/socioeconomic conditions; counterfactual modeling of harm reduction under alternative regulatory regimes (Australia, Canada, UK baseline comparison)',
    'If proliferation-attributable harm is >30% of total: the victim set classification is justified. If <10%: the tangled rope classification for moderate communities should shift toward rope (less asymmetric extraction). This determines the severity of suppression and scope of victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harm_asymmetry_measurement, empirical, 'Causal attribution of harm to firearms proliferation vs. other structural factors').

omega_variable(
    judicial_doctrine_coherence,
    'Can the lower federal judiciary''s upholding of most existing gun regulations be coherently justified under a genuine individual rights framework, or does it constitute contradiction (piton signature)?',
    'Doctrinal analysis: enumeration of all post-Heller regulations affirmed by federal courts with stated justifications; comparison to the original Heller framework for coherence and consistency; identification of patterns in which regulations pass and which fail',
    'If doctrine is coherent: the piton classification is incorrect; courts are genuinely applying meaningful scrutiny. If incoherent (regulations pass by narrow margin and ad-hoc reasoning): piton is confirmed — institutional actors are maintaining contradictory logics to avoid conflict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_doctrine_coherence, empirical, 'Coherence of post-Heller judicial doctrine').

omega_variable(
    natural_rights_grounding,
    'Is the individual rights reading genuinely grounded in natural rights philosophy, or does it rest on 20th-century originalism as an interpretive method?',
    'Historical analysis of natural rights tradition (Locke, Blackstone, founding era texts) and comparison to Heller''s reasoning; identification of whether the reading emerges from philosophical premises or from judicial methodology',
    'If grounded in natural rights: mountain classification has validity (foundation is more stable than interpretive choice). If grounded in originalism: the reading is a methodological artifact, not a discovery of pre-political law. False summit triggers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_rights_grounding, conceptual, 'Whether natural rights grounding is genuine or methodological artifact').

omega_variable(
    scaffold_sunset_mechanism,
    'What factual or political changes would be sufficient to trigger reinterpretation of the Second Amendment away from individual rights doctrine?',
    'Monitoring of Supreme Court composition shifts, mass shooting frequency/public opinion correlation, successful amendment efforts, international comparative pressure, domestic violence prosecution trends',
    'If sunset is plausible: scaffold classification is justified. If Supreme Court entrenchment and political mobilization make reinterpretation implausible: scaffold should downgrade to tangled_rope with permanent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scaffold_sunset_mechanism, preference, 'Plausibility of sunset mechanism for constitutional reinterpretation').

omega_variable(
    domestic_violence_exception_adequacy,
    'Does the existing 922(g)(8) exception for abusers under protective orders sufficiently protect intimate-partner victims, or does the individual rights reading still create structural extraction?',
    'Empirical analysis: rate of intimate-partner firearms homicides pre- vs. post-922(g)(8); enforcement data on protective orders; analysis of cases where abusers retained access despite order or threat assessment',
    'If exception is adequate and well-enforced: victim classification should downgrade, snare → tangled rope. If exception is narrow and poorly enforced: snare classification is confirmed and extraction remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_violence_exception_adequacy, empirical, 'Adequacy of domestic violence exception under 922(g)(8)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__individual_rights_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sa_ind_tr_t0, second_amendment_text__individual_rights_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(sa_ind_tr_t5, second_amendment_text__individual_rights_reading, theater_ratio, 5, 0.62).
narrative_ontology:measurement(sa_ind_tr_t10, second_amendment_text__individual_rights_reading, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(sa_ind_be_t0, second_amendment_text__individual_rights_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sa_ind_be_t5, second_amendment_text__individual_rights_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(sa_ind_be_t10, second_amendment_text__individual_rights_reading, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sa_ind_su_t0, second_amendment_text__individual_rights_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(sa_ind_su_t5, second_amendment_text__individual_rights_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(sa_ind_su_t10, second_amendment_text__individual_rights_reading, suppression_requirement, 10, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__individual_rights_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_text__individual_rights_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_text__individual_rights_reading, second_amendment_text__collective_rights_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_rights_reading, second_amendment_text__hybrid_civic_reading).
narrative_ontology:affects_constraint(second_amendment_text__individual_rights_reading, intimate_partner_femicide_structural).
narrative_ontology:affects_constraint(second_amendment_text__individual_rights_reading, mass_shooting_externality).
narrative_ontology:affects_constraint(second_amendment_text__individual_rights_reading, firearms_proliferation_externality).

% DUAL FORMULATION NOTE:
% The Second Amendment is a contested kernel with multiple structurally distinct readings. This file instantiates the individual_rights_reading (ε=0.58, Tangled Rope). Sibling readings (collective_rights, hybrid_civic) have different ε values and different victim/beneficiary structures and should be modeled as separate constraint stories linked via network.affects_constraints. The individual rights reading is upstream of intimate partner femicide and mass shooting externality constraints — both downstream constraints are enabled by this reading's structural consequences. The reading also influences firearms proliferation externality constraint by removing legal barriers to market expansion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_text__individual_rights_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
