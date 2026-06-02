% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary (Restrictive Anthropocentric Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   The legal personhood boundary, restricted to born humans with cognitive
 *   capacity, represents one specific reading of a deeply contested kernel —
 *   the question of which entities deserve legal recognition as subjects of
 *   rights rather than objects of property. This restrictive reading excludes
 *   fetuses (by the 'born' criterion), non-human animals (by the 'human'
 *   criterion), ecosystems (by the entity-boundedness requirement), and
 *   artificial intelligences (by the biological-origin implicit criterion).
 *   The boundary serves genuine coordination functions: it provides doctrinal
 *   clarity for property law, contract law, and inheritance regimes, enabling
 *   stable rules for succession and commerce. However, it also functions as
 *   an extraction mechanism: by limiting personhood to born humans with
 *   cognitive capacity, the reading preserves state authority to regulate
 *   reproduction, subordinate pregnant person autonomy, exclude non-human
 *   systems from moral standing, and forestall AI legal status claims. The
 *   constraint's extractiveness (0.58) reflects moderate but asymmetric harm:
 *   the pregnant person faces maximum extraction (bodily autonomy
 *   subordinated), the state experiences negative extraction (captures
 *   legitimacy for reproductive regulation), and non-human systems experience
 *   exclusion-as-extraction (assigned no standing despite often being
 *   stakeholders in legal disputes). The suppression value (0.72) reflects
 *   strong institutional enforcement of the anthropocentric boundary through
 *   case law, legislation, enforcement action, and doctrinal assumptions
 *   baked into legal education and practice. The theater ratio (0.48)
 *   indicates that the boundary is enforced through relatively
 *   straightforward doctrinal means (birth certificates, cognitive
 *   assessments, species membership) rather than elaborate performative
 *   ritual — the boundary itself functions as theater insofar as it
 *   naturalizes what is actually a contingent institutional choice.
 *
 * KEY AGENTS:
 *   - Pregnant Persons: Primary victims (powerless/trapped) — experience maximum extraction as bodily autonomy is subordinated to fetal personhood claims throughout pregnancy
 *   - State Reproductive Authority: Primary beneficiary (institutional/arbitrage) — captures regulatory power over reproduction by invoking fetal personhood; negative effective extraction
 *   - Fetal Development Interests: Complex beneficiary (moderate/constrained) — legitimate coordination function for protecting viable development, but invoked beyond viability to suppress pregnant person autonomy
 *   - Non-Human Animals and Ecosystems: Secondary victims (powerless/trapped) — excluded from personhood, assigned property status, denied legal standing; face extraction through environmental exploitation justified by non-personhood status
 *   - Constitutional Courts: Institutional interpreters (institutional/constrained) — experience constrained exit from anthropocentric doctrine; can move sideways (limited non-human personhood in specific contexts) but cannot directly breach the boundary without doctrinal destabilization
 *   - Rights Expansion Movements: Organized challengers (organized/mobile) — reproductive rights, animal rights, environmental rights, AI ethics coalitions see the boundary as eroding; have alternatives available in some jurisdictions
 *   - Anthropocentric Legal Doctrine: Beneficiary abstraction (institutional/arbitrage) — the doctrine itself benefits from the boundary by preserving its coherence; doctrine persists through inertia and institutional investment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.58).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.72).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary (Restrictive Anthropocentric Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, 'aad8dfdb-7c30-414b-92fa-34e37ba5c086').
narrative_ontology:cs_kernel_codification('aad8dfdb-7c30-414b-92fa-34e37ba5c086', formalized).
narrative_ontology:cs_authority_grounding('aad8dfdb-7c30-414b-92fa-34e37ba5c086', lineage).
narrative_ontology:cs_interpretation_layer_present('aad8dfdb-7c30-414b-92fa-34e37ba5c086').
narrative_ontology:cs_reading_relation('aad8dfdb-7c30-414b-92fa-34e37ba5c086', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_reading_relation('aad8dfdb-7c30-414b-92fa-34e37ba5c086', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('aad8dfdb-7c30-414b-92fa-34e37ba5c086', foundational, human_species_membership_boundary).
narrative_ontology:cs_axiom_status(human_species_membership_boundary, holdable).
narrative_ontology:cs_axiom_grounding('aad8dfdb-7c30-414b-92fa-34e37ba5c086', human_species_membership_boundary, conventional).
narrative_ontology:cs_axiom('aad8dfdb-7c30-414b-92fa-34e37ba5c086', foundational, cognitive_capacity_threshold).
narrative_ontology:cs_axiom_status(cognitive_capacity_threshold, holdable).
narrative_ontology:cs_axiom_grounding('aad8dfdb-7c30-414b-92fa-34e37ba5c086', cognitive_capacity_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('aad8dfdb-7c30-414b-92fa-34e37ba5c086', anthropocentric_natural_law_personhood).
narrative_ontology:cs_drift_state('aad8dfdb-7c30-414b-92fa-34e37ba5c086', contemporary_post_reproductive_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aad8dfdb-7c30-414b-92fa-34e37ba5c086', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, state_reproductive_authority).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, anthropocentric_legal_doctrine).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_development_interests).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecological_systems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PREGNANT PERSON (SNARE) — Trapped by the personhood boundary that excludes the fetus, converting bodily autonomy into a contested zone where the pregnant person's will is subordinated to fetal interests. State enforces fetal personhood claims against the pregnant person's constitutional agency. Maximum extraction: the pregnant person loses sovereignty over their own body for 9 months and beyond, with no exit option. Classification reflects that the pregnant person experiences this constraint as pure extraction — coordination functions (supporting child development) are presented as justification, but the actual mechanism is suppression of pregnant person's legal status.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FETAL DEVELOPMENT INTERESTS (TANGLED ROPE) — Experiences coordination (protection of viable fetal development is a genuine social function) alongside extraction (fetal personhood claims are often weaponized to override pregnant person autonomy beyond viability thresholds). Exit is constrained by biological dependency. The fetus itself has no independent perspective; this represents the state's invocation of fetal interests as a proxy. Real coordination function exists (protecting viable pregnancy), but it is coercively extended to cover non-viability periods.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE REPRODUCTIVE AUTHORITY (ROPE) — Experiences the personhood boundary as pure coordination: the state solves the legitimacy problem of regulating reproduction by invoking fetal personhood. The boundary enables state authority to penetrate intimate domains (contraception access, pregnancy termination, medical decision-making during pregnancy) by reframing these as disputes between legal persons rather than as state intrusions into pregnant person autonomy. State experiences this as low extraction because it frames reproductive regulation as neutral arbitration between two rights-holders. Effective extraction χ is negative: the state captures legitimacy and authority through the constraint.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ECOLOGICAL AND NON-HUMAN SYSTEMS (PITON) — Excluded from personhood boundary, reducing to property status. The constraint's theater: legal doctrine performs due consideration of environmental and animal interests (standing doctrines, environmental impact statements, animal welfare regulations) while the fundamental classification (not persons) undermines any claim to intrinsic moral status. The organized advocacy movements for ecological and non-human personhood see the boundary as a degraded institutional arrangement maintained through inertia — the functionalist and developmental readings would relocate boundary to include some non-human actors, but anthropocentric doctrine persists despite mounting structural pressures. Theater ratio reflects performative environmental law and animal protection that leaves core personhood untouched.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RIGHTS-BASED EXPANSION MOVEMENTS (SCAFFOLD) — Organized actors (reproductive rights coalitions, animal rights advocates, environmental organizations, AI ethics initiatives) see the restrictive boundary as a temporary institutional fixture being systematically challenged and reconstructed. Mobile exit: jurisdictions recognizing bodily autonomy for pregnant persons, animal personhood in specific legal contexts, ecosystem legal standing, and potential AI personhood frameworks provide alternatives. Sunset logic: as reproductive autonomy norms mature, fetal personhood claims become less enforceable; as non-human personhood is recognized in parallel legal systems (Ecuador, India, New Zealand), the anthropocentric monopoly erodes. Effective extraction is low because organized agents see and can partially escape the boundary.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSTITUTIONAL COURTS (TANGLED ROPE) — Institutional interpreters experience the personhood boundary as both coordination and extraction. Genuine coordination function: courts must adjudicate reproduction, environmental, and AI rights claims using some coherent framework; the anthropocentric boundary provides doctrinal clarity. Embedded extraction: courts are constrained by constitutional lineage (inherited doctrine from eras of unrestricted reproductive control and non-human property status), limiting the interpretive range available to them. Courts cannot simply erase the boundary without destabilizing the entire doctrinal edifice grounded in anthropocentrism. Exit is constrained: moving to functional or potentiality readings would require rewriting vast bodies of property law, environmental law, and constitutional precedent. Some courts have begun sideways moves (recognizing limited non-human personhood in specific contexts) rather than direct boundary relocation.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: NATURAL LAW / ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, human cognitive capacity and biological origin constitute an irreducible boundary for moral and legal personhood. This reading treats the boundary as a discovered feature of human nature, not a constructed institutional choice. However, this classification will trigger false-summit detection: identifiable beneficiaries (state reproductive authority, anthropocentric doctrine) and the constraint's high suppression and extractiveness undermine the mountain claim. The analytical perspective instantiates the oracle gap: the framework itself prevents the natural-law perspective from seeing the institutional structure it depends on.
constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_personhood_boundary__restrictive_anthropocentric_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, TR),
    TR >= 0.70.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The restrictive reading extracts moderate but significant value from pregnant persons through subordination of bodily autonomy. The extraction is not total (pregnant persons retain some legal rights, informed consent requirements apply in many jurisdictions, health exceptions exist) but is systematic. The measurement trajectory shows increasing extractiveness from 0.42 → 0.58 over the interval, reflecting intensifying efforts to enforce fetal personhood even in early pregnancy (heartbeat bills, conception-to-birth restrictions) — the extractive function is ramping up. Suppression (0.72): High and increasing. Institutional enforcement of the anthropocentric boundary includes legislation, case law, enforcement action, and institutional inertia in legal education and practice. The boundary is enforced through direct prohibition (restricting abortion access, denying non-human standing in court), economic barriers (concentrating reproductive healthcare, making alternatives inaccessible), and epistemic closure (treating the boundary as natural law rather than contested doctrine). Theater ratio (0.48): Moderate. The boundary's enforcement involves less elaborate performative ritual than some constraints — birth certificates and cognitive assessments are relatively straightforward doctrinal tools. However, the boundary itself functions as theater insofar as it naturalizes what is actually a contingent institutional choice. The theater ratio increases from 0.38 → 0.48 over the interval, reflecting increasing reliance on rhetorical claims about natural boundaries as legal enforcement intensifies (suggesting that direct enforcement is becoming insufficient).
 *
 * PERSPECTIVAL GAP:
 *   The restrictive reading produces a stark perspectival gap: pregnant persons see a snare (pure extraction of autonomy), the state sees a rope (coordination mechanism for regulating reproduction), courts see a tangled rope (genuine coordination for clarity alongside constrained interpretive options), and the analytical observer risks seeing a mountain (natural law) until the false-summit detector fires based on identifiable beneficiaries and high suppression. The gap reflects genuine structural differences in how each agent experiences the boundary: to the pregnant person, the boundary is the cage; to the state, it is the key to legitimate authority; to the court, it is both framework and constraint; to the analytical observer, it appears natural until the engine computes its dependency on beneficiaries. This gap is diagnostic of the boundary's constructed nature — a genuinely natural law would produce convergent perspectives across all observer positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position relative to the personhood boundary and the flow of extraction. Pregnant persons occupy full target position: the boundary directly extracts their autonomy; they have trapped exit options; they bear costs with no choice. State reproductive authority occupies full beneficiary position: the boundary enables regulatory power; they have arbitrage options (can leverage the boundary for other purposes); they experience negative extraction (gain authority/legitimacy). The fetal development interest, though abstractly presented, occupies a partial beneficiary position: viability-protecting functions are genuine; fetal interests are partially recognized as rights-holders. The non-human systems occupy full target position: excluded from personhood, assigned property status, trapped. The institutional courts occupy constrained beneficiary position: benefit from doctrinal clarity but cannot exit the framework without institutional destabilization. The analytical observer occupies analytical position but risks naturalizing the boundary (false summit risk). The directionality for each perspective feeds into the engine's f(d) sigmoid function to compute experienced chi at that context.
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive anthropocentric reading resolves the mandatrophy by demonstrating that the classification is stable across perspectives IF we accept the reading's core axiom (human biological origin + cognitive capacity = natural boundary). However, the omega variables reveal that this axiom is empirically and conceptually contested. The developmental potentiality reading would shift the boundary backward (extending personhood to conception or neural formation), reclassifying pregnant persons' autonomy from snare to constrained/tangled_rope. The functional capacity reading would expand personhood to non-human entities with decision-making or ecological function, reclassifying non-human systems from property to victim status. The restrictive reading's persistence despite these challenges reflects institutional capture (anthropocentric doctrine's investment in its own continuity) and state interest in reproductive control, not discovery of a natural boundary. The mandatrophy is resolved not by demonstrating this reading is correct, but by recognizing that the classification (snare for pregnant persons, rope for state) is stable within this reading's axioms and remains empirically testable against the alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fetal_viability_threshold_contingency,
    'Is the boundary between fetal non-personhood and personhood naturally discovered at viability, or is viability a contingent technological/medical threshold that shifts as neonatal medicine advances?',
    'Historical analysis of viability definitions; comparative law jurisdictions using different thresholds (conception, heartbeat, neural activity, viability, birth); correlation between viability threshold changes and medical capability changes',
    'If naturally discovered: boundary is intrinsic to fetal development. If contingent: boundary is socially constructed and sensitive to technical capacity — undermines mountain classification, supports tangled_rope/snare. The restrictive reading''s core axiom (born humans with cognitive capacity) requires birth as a natural boundary; if birth timing is technologically contingent, the axiom loses natural-law grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fetal_viability_threshold_contingency, empirical, 'Whether viability threshold is natural or technologically contingent').

omega_variable(
    cognitive_capacity_definitional_instability,
    'Can ''cognitive capacity'' be defined with sufficient clarity and stability to function as a boundary criterion, or does every proposed definition exclude marginal cases (infants, severe dementia, persistent vegetative states) that legal personhood actually protects?',
    'Examination of cognitive capacity definitions across neuroscience, psychology, and law; application of proposed definitions to edge cases; review of personhood court cases involving cognitive impairment or developmental delay',
    'If definable: capacity can ground personhood boundary. If indefinable or excludes legal persons: the restrictive reading''s core axiom collapses — cognitive capacity cannot serve as a natural boundary if it excludes humans the reading claims to protect. Suggests reading is internally incoherent, not just contested.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_capacity_definitional_instability, conceptual, 'Whether cognitive capacity can serve as a stable personhood criterion').

omega_variable(
    extraction_mechanism_vs_genuine_fetal_interests,
    'How much of the state''s invocation of fetal personhood reflects legitimate fetal developmental interests, and how much reflects state interest in controlling pregnant person sexuality, reproduction, and bodily autonomy?',
    'Comparative analysis: fetal protection measures that include pregnant person autonomy/welfare (prenatal care access, harm prevention, post-birth support) vs measures that override pregnant person autonomy without regard to fetal welfare; temporal analysis of fetal personhood claims (rising after reproductive rights victories, suggesting reaction formation rather than organic development); state investment patterns (reproductive regulation vs neonatal care, parental support)',
    'If mostly genuine fetal interests: tangled_rope classification appropriate — coordination function is real. If mostly state authority expansion: snare classification applies — the fetal personhood claim is a mechanism for reproductive control. This omega is the key to mandatrophy: does the extracted value flow to fetal development or to state authority?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_vs_genuine_fetal_interests, empirical, 'Proportion of fetal personhood invocation driven by state authority interest vs genuine fetal welfare').

omega_variable(
    non_human_personhood_precedent_equivalence,
    'Do existing legal recognitions of non-human personhood (corporations, trusts, ecosystems, animals in some jurisdictions) establish that the restrictive anthropocentric boundary is contingent, or are these exceptional extensions that preserve the anthropocentric core?',
    'Comparative law analysis of non-human personhood regimes; examination of whether these regimes are genuinely expanding personhood or creating subsidiary categories; investigation of whether legal systems recognize animal/ecosystem personhood alongside or in lieu of human personhood; analysis of lobbying and institutional resistance to non-human personhood expansion',
    'If genuinely expanding: the anthropocentric boundary is already breached and contingent — supports functional or broader readings. If exceptional only: anthropocentrism remains the doctrinal core — supports continued restrictive reading dominance but weakens the natural-law grounding (if the boundary were truly natural, why the need for exceptions?). This omega drives the piton perspective''s theater_ratio observation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_human_personhood_precedent_equivalence, empirical, 'Whether non-human personhood precedents undermine anthropocentric boundary').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Do the developmental potentiality and functional capacity readings logically foreclose the restrictive anthropocentric reading, or can multiple readings coexist in different legal jurisdictions and constitutional frameworks?',
    'Examination of whether the three readings are mutually exclusive or can be held simultaneously within a single legal system; comparative analysis of jurisdictions using different readings; study of constitutional amendment history to determine whether adoption of potentiality/functional readings requires explicit rejection of anthropocentrism',
    'If forecloses: the restrictive reading''s persistence indicates institutional capture or resistance to logical coherence. If coexists: multiple readings can partition the problem space (e.g., personhood for property/contract law vs functional criteria for rights law) — suggests institutional fragmentation rather than unified doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether sibling readings logically foreclose restrictive anthropocentric reading').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the observed suppression of pregnant person autonomy reflects external legal enforcement, and how much reflects internalized norms where pregnant persons themselves accept fetal personhood claims as legitimate?',
    'Comparative study of abortion access laws and actual abortion rates; analysis of pregnant persons'' stated reasons for accepting/rejecting fetal personhood claims; longitudinal tracking of norm shifts as reproductive autonomy messaging increases; cross-cultural comparison of suppression levels',
    'If mostly external enforcement: suppression metric is accurate as structural measure. If significantly internalized: the true suppression experienced by pregnant persons exceeds the structural measure — identity-locked classification becomes more appropriate than trapped. This affects the stage at which identity-fusion intervention would be required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Extent to which suppression is externally enforced vs internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lpb_restrict_tr_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(lpb_restrict_tr_t25, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(lpb_restrict_tr_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(lpb_restrict_be_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lpb_restrict_be_t25, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(lpb_restrict_be_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lpb_restrict_su_t0, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(lpb_restrict_su_t25, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(lpb_restrict_su_t50, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary__functional_capacity_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, fetal_personhood_doctrine).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, reproductive_autonomy_enforcement).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animal_legal_status).
narrative_ontology:affects_constraint(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystem_standing_doctrine).

% DUAL FORMULATION NOTE:
% The legal personhood boundary is a single contested kernel with three structurally distinct readings. Each reading yields different beneficiary/victim structures, different extractiveness values, and different perspectives. The restrictive anthropocentric reading (this file) excludes fetuses, non-human animals, ecosystems, and AIs from personhood. The developmental potentiality reading extends personhood backward to conception or neural formation. The functional capacity reading extends personhood to non-human entities with decision-making capacity or ecological function. These three readings are not one constraint viewed from different angles — they have different ε values reflecting different empirical and normative claims about where the boundary naturally or should be located. Each reading is its own constraint story with its own perspectives and omegas documenting the shared uncertainty about boundary location.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__restrictive_anthropocentric_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
