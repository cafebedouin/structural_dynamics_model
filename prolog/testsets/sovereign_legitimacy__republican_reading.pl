% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__republican_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__republican_reading, []).

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
 *   constraint_id: sovereign_legitimacy__republican_reading
 *   human_readable: Republican Legitimacy: Sovereign Authority by Popular Delegation and Revocable Consent
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The republican reading of sovereign legitimacy asserts that all authority
 *   derives from the consent of the governed and remains revocable by popular
 *   will. This is one reading of the contested kernel 'sovereign
 *   legitimacy'—a foundational claim about where authority gets its
 *   justification. The reading generates a tangled_rope constraint: it
 *   contains a genuine coordination function (the periodic election as a
 *   legitimacy test, the prohibition on hereditary usurpation) alongside
 *   asymmetric extraction (the systematic exclusion of non-voting populations
 *   from the consent framework while being governed by it, the suppression of
 *   alternative authority sources, the theater of participation). Over two
 *   centuries, the extractiveness value has risen modestly (0.22 to 0.38) as
 *   the electorate has expanded but formal participation barriers have
 *   shifted rather than dissolved. Suppression has fallen (0.68 to 0.52) as
 *   suffrage expanded, but theater has risen (0.38 to 0.58) as electoral
 *   authenticity has degraded relative to the principle of consent. The
 *   constraint's structural history reflects the gap between the reading's
 *   core claim (legitimate authority requires ongoing popular mandate) and
 *   the institutional reality (elections often function as legitimacy ritual
 *   rather than genuine consent-testing).
 *
 * KEY AGENTS:
 *   - Electorate (enfranchised): Primary beneficiary (institutional/arbitrage) — authors sovereign will in theory; granted periodic legitimacy veto power
 *   - Disenfranchised populations: Primary victims (powerless/trapped) — governed by authority claiming democratic legitimacy while systematically excluded from consent mechanisms
 *   - Republican government: Secondary beneficiary (institutional/immediate) — exercises authority while citing popular mandate; benefits from election-cycle legitimacy validation
 *   - Hereditary power structures: Victim-by-displacement (institutional/arbitrage) — logically foreclosed by the consent reading; relegated to ceremonial function or eliminated entirely
 *   - Democratic reform movements: Organized agents (organized/mobile) — see the reading as containing its own fulfillment mechanism (expand electorate, the extraction gap closes)
 *   - Analytical observer: Constitutionally embedded (analytical/identity_locked) — observes from within the democratic tradition; cannot externally assess the framework's contingency
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__republican_reading, 0.38).
domain_priors:suppression_score(sovereign_legitimacy__republican_reading, 0.52).
domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sovereign_legitimacy__republican_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__republican_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__republican_reading, "Republican Legitimacy: Sovereign Authority by Popular Delegation and Revocable Consent").
narrative_ontology:topic_domain(sovereign_legitimacy__republican_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__republican_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__republican_reading, '04ee418a-62e7-404c-826d-43b4e47024c5').
narrative_ontology:cs_kernel_codification('04ee418a-62e7-404c-826d-43b4e47024c5', fixed_text).
narrative_ontology:cs_authority_grounding('04ee418a-62e7-404c-826d-43b4e47024c5', lineage).
narrative_ontology:cs_interpretation_layer_present('04ee418a-62e7-404c-826d-43b4e47024c5').
narrative_ontology:cs_reading_relation('04ee418a-62e7-404c-826d-43b4e47024c5', sovereign_legitimacy__monarchical_reading, forecloses).
narrative_ontology:cs_reading_relation('04ee418a-62e7-404c-826d-43b4e47024c5', sovereign_legitimacy__constitutional_hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('04ee418a-62e7-404c-826d-43b4e47024c5', foundational, all_authority_requires_popular_consent).
narrative_ontology:cs_axiom_status(all_authority_requires_popular_consent, holdable).
narrative_ontology:cs_axiom_grounding('04ee418a-62e7-404c-826d-43b4e47024c5', all_authority_requires_popular_consent, deontological).
narrative_ontology:cs_axiom('04ee418a-62e7-404c-826d-43b4e47024c5', foundational, consent_must_be_periodic_and_revocable).
narrative_ontology:cs_axiom_status(consent_must_be_periodic_and_revocable, holdable).
narrative_ontology:cs_axiom_grounding('04ee418a-62e7-404c-826d-43b4e47024c5', consent_must_be_periodic_and_revocable, instrumental).
narrative_ontology:cs_axiom('04ee418a-62e7-404c-826d-43b4e47024c5', secondary, hereditary_succession_insufficient_for_legitimacy).
narrative_ontology:cs_axiom_status(hereditary_succession_insufficient_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('04ee418a-62e7-404c-826d-43b4e47024c5', hereditary_succession_insufficient_for_legitimacy, deontological).
narrative_ontology:cs_reference_frame('04ee418a-62e7-404c-826d-43b4e47024c5', democratic_popular_sovereignty).
narrative_ontology:cs_drift_state('04ee418a-62e7-404c-826d-43b4e47024c5', contemporary_surveillance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('04ee418a-62e7-404c-826d-43b4e47024c5', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__republican_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, electorate).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__republican_reading, democratic_institutions).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, hereditary_claimants).
narrative_ontology:constraint_victim(sovereign_legitimacy__republican_reading, non_voting_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED MAJORITY (SNARE) — Trapped within the electoral consent framework but excluded from participation. The claim that legitimacy derives from consent operates as extraction: authority invokes popular sovereignty as justification while systematically denying voting access to entire populations (historical: women, enslaved people, landless workers; contemporary: non-citizens, felons). The constraint suppresses alternatives (exit into alternative governance forms) while extracting compliance from those it formally excludes from consent.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ENFRANCHISED ELECTORATE (TANGLED ROPE) — Constrained by periodic election cycles and majoritarian voting rules (can exit via emigration at high cost; migration barriers remain substantial). Experiences both coordination and extraction: genuine periodic legitimacy test exists (coordination function), but ballot access, gerrymandering, and campaign finance create asymmetric extraction. Beneficiary of the framework in theory (author of sovereign will), but many voters experience suppression through procedural barriers.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REPUBLICAN GOVERNMENT (ROPE) — Benefits from the legitimacy framework: can exercise authority while citing popular mandate. Experiences the constraint as pure coordination — the election cycle and popular-consent doctrine enable governance by providing periodic consent validation and preventing dynastic usurpation. The government has arbitrage capacity (can revise electoral rules, implement alternative legitimacy doctrines) but treats the consent framework as binding through ideology and international recognition.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEREDITARY POWER STRUCTURE (PITON) — This reading forecloses hereditary legitimacy by asserting consent as the sole source. Hereditary claimants are reduced to historical vestiges in republican systems — formal authority may be retained (British monarchy) but is theatrically reframed as delegated from popular will rather than inherited divine right. The hereditary structure persists through institutional inertia and ceremonial function despite functional foreclosure.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: DEMOCRATIC REFORM MOVEMENT (SCAFFOLD) — Organized agents (suffragist movements, voting rights coalitions, election integrity organizations) see the consent framework as containing its own sunset logic. If the doctrine is that authority requires popular legitimacy, then expanding the electorate to include all affected populations is not amendment but fulfillment of the principle. The constraint is temporary — once universal suffrage and full participation are achieved, the extraction mechanism (the exclusion gap between claim and practice) dissolves. Theater drops as participation expands.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IDENTITY-LOCKED (TANGLED ROPE) — The observer's analytical position is itself constituted through the republican tradition. The principle of democratic legitimacy is not externally observed but internally inhabited — the analyst is educated within democratic institutions, trained in constitutional law, and professionally committed to the legitimacy of electoral consent. The analytical frame cannot see the constraint as contingent because the frame itself IS the constraint. This perspective instantiates the oracle gap (Theorem 4): the native instruments (electoral theory, constitutional analysis) cannot detect the structure that cross-cultural or historical distance reveals.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW VIEW / UNIVERSAL CONSENT (MOUNTAIN) — From a universal philosophical perspective, consent may appear immutable: all legitimate authority must derive from the will of the governed (this is the core axiom). This is presented as a logical necessity, not a contingent institutional feature. However, this classification is vulnerable to false-summit detection: beneficiaries are identifiable (democratic elites, enfranchised populations, republican institutions), and the constraint can be historicized (it emerges as specific institutional arrangements, not as a priori truth). The engine will reclassify this as tangled_rope masked by naturalization.
constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__republican_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_legitimacy__republican_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereign_legitimacy__republican_reading, TR),
    TR >= 0.70.

:- end_tests(sovereign_legitimacy__republican_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does establish genuine coordination (elections, consent-based legitimacy, prohibition on hereditary succession) but creates substantial extraction through the inclusion gap: authority derives legitimacy from popular consent while systematically withholding the capacity to consent from entire populations. The value reflects both functions operating simultaneously. The measurement trajectory (0.22→0.38) shows extractiveness rising as the gap between proclaimed consent and actual participation has widened—modern systems claim broader legitimacy than 18th-century ones but deliver less authentic participation relative to claim. Suppression (0.52): Moderate-high. The constraint suppresses alternatives (hereditary authority, autocratic rule, alternative legitimacy doctrines) and creates barriers to exit (emigration costs, international non-recognition of rival sovereigns). However, suppression has fallen over time as suffrage expansion has reduced the population trapped outside the consent mechanism. Current suppression reflects remaining barriers: gerrymanders, campaign finance, ballot access restrictions. Theater ratio (0.58): Moderate-high. Elections serve a genuine coordination function but have acquired substantial performative content: legitimacy theater becomes more important when electoral authenticity declines. Measurements show theater rising (0.38→0.58) as the electoral system has become more technically sophisticated (messaging, targeting, professionalization) while actual voter agency relative to policy outcomes has arguably declined. The reading's own internal standard—that legitimate authority requires ongoing popular mandate—renders the system's electoral authenticity diagnostically crucial.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal how the same structural claim (legitimate authority requires consent) produces radically different experienced constraints depending on structural position. The disenfranchised majority experiences the consent doctrine as snare: invoked to justify their governance while denying them participation. The enfranchised electorate experiences tangled rope: genuine periodic legitimacy test but constrained by majoritarian rules and participation barriers. The republican government experiences rope: coordination mechanism that enables stable authority. The hereditary structure experiences piton: logically foreclosed but institutionally persisting through ceremonial and cultural inertia. The reform movement experiences scaffold: sees the reading as containing its own sunset (expand the electorate, the extraction gap closes). The analytical observer experiences identity-locked tangled rope: observes the constraint from within the democratic tradition, unable to see its contingency. The natural law view risks false-summit classification: would present consent as an immutable requirement, but beneficiaries and historical specificity reveal it as a constructed, contestable claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and structural position. The electorate (electorate, democratic_institutions) are formal beneficiaries but experience constrained, not arbitrage, exit—they cannot easily change the consent framework itself. Hereditary claimants are victims of this reading's core premise (displaced as authority source), but they occupy institutional power levels and historically had arbitrage capacity (could resist revolution, negotiate constitutional settlements). The disenfranchised are trapped victims (powerless, cannot exit the governed population). The analytical observer's identity_locked position reflects that the observer's professional identity is constituted through democratic theory—the inability to externally assess the framework's contingency is built into the role. All directionality derivations flow from these structural facts; no overrides are required.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_measurement_ambiguity,
    'What constitutes genuine popular consent? Is a single election every N years sufficient, or does ongoing participation/regular mandate renewal represent the true consent requirement?',
    'Historical analysis of consent doctrines: direct democracy vs representative intervals; comparison of legitimacy crises in systems with vs without recall/initiative mechanisms; measurement of compliance and active vs passive acceptance',
    'If single election suffices: constraint is lower extraction (Rope). If ongoing consent required: current systems fail the consent test, reclassifying as Snare (falsely claiming legitimacy while denying actual participation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_measurement_ambiguity, conceptual, 'Definition of what constitutes sufficient popular consent').

omega_variable(
    consent_threshold_inclusion,
    'Does legitimate authority require consent from ALL affected populations, or only from citizens/property-holders/voters of a specific category? Where is the inclusion threshold?',
    'Historical reconstruction of franchise boundaries at each constitutional moment; analysis of justifications for exclusion (literacy, gender, property, citizenship status); identification of when expansion occurred and on what legitimacy grounds',
    'If universal inclusion required: all historical republican systems are snares (claim legitimacy through consent while systematically excluding affected populations). If categorical consent suffices: constraint can be rope at narrower scope. The reading''s core claim that ''legitimate authority requires consent'' generates irresolvable tension with bounded franchise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_threshold_inclusion, conceptual, 'Inclusion threshold for whose consent constitutes legitimacy').

omega_variable(
    revocability_enforcement_gap,
    'Can popular consent actually be revoked without revolution? Do republican systems contain peaceful mechanisms for total authority revocation, or is revocability theoretical only?',
    'Comparative analysis of constitutional amendment procedures, recall mechanisms, and impeachment frameworks; historical documentation of peaceful vs violent regime changes in republican systems; assessment of formal mechanisms vs actual practice',
    'If revocability is enforced: constraint is lower extraction (true periodic legitimacy test). If revocability is theoretical/blocked: constraint is snare (consent is invoked but cannot be exercised, suppressing alternatives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revocability_enforcement_gap, empirical, 'Whether popular consent can actually be revoked without revolution').

omega_variable(
    reading_contest_in_kernel,
    'This reading asserts that legitimate authority derives from consent. How does this reading''s core premise relate to and constrain the monarchical reading (hereditary authority derived from divine right or natural order) and the constitutional hybrid reading (authority derived from constitutional settlement between monarch and people)?',
    'Logical analysis of the axioms: does consent-based legitimacy logically foreclose hereditary legitimacy? Or do these readings coexist as live positions held by different parties and traditions? What would it mean for both to be true simultaneously in a single framework?',
    'If the readings foreclose each other: the kernel is a genuine normative battleground where only one can prevail. If they coexist: the kernel is under-specified and different parties instantiate different readings institutionally. If hybrid is possible: this reading forecloses pure monarchy but coexists with constitutionalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_in_kernel, conceptual, 'Logical relationships between republican, monarchical, and hybrid readings of sovereign legitimacy').

omega_variable(
    electoral_authenticity_performance,
    'Do contemporary electoral systems function as authentic consent-gathering mechanisms, or have they degraded into performative legitimacy theater that maintains the appearance of consent while suppressing actual participation?',
    'Measurement of voter turnout trends, non-voting populations, barriers to ballot access, campaign finance capture, gerrymandering effects; comparison of formal electoral rights vs actual participation capacity; analysis of whether election outcomes align with measured public preferences or diverge systematically',
    'If authentic: theater_ratio is lower, classification remains tangled_rope or rope. If performative: theater_ratio is higher, constraint reclassifies toward piton or snare (ritual replaces function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_authenticity_performance, empirical, 'Whether elections function as authentic consent or as legitimacy performance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__republican_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1789, sovereign_legitimacy__republican_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(theater_1889, sovereign_legitimacy__republican_reading, theater_ratio, 100, 0.52).
narrative_ontology:measurement(theater_1989, sovereign_legitimacy__republican_reading, theater_ratio, 200, 0.58).

% Extraction over time
narrative_ontology:measurement(extractiveness_1789, sovereign_legitimacy__republican_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(extractiveness_1889, sovereign_legitimacy__republican_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement(extractiveness_1989, sovereign_legitimacy__republican_reading, base_extractiveness, 200, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(suppression_1789, sovereign_legitimacy__republican_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(suppression_1889, sovereign_legitimacy__republican_reading, suppression_requirement, 100, 0.58).
narrative_ontology:measurement(suppression_1989, sovereign_legitimacy__republican_reading, suppression_requirement, 200, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__republican_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__monarchical_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, sovereign_legitimacy__constitutional_hybrid_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, electoral_authenticity_crisis).
narrative_ontology:affects_constraint(sovereign_legitimacy__republican_reading, suffrage_expansion_as_structural_fulfillment).

% DUAL FORMULATION NOTE:
% The sovereign legitimacy kernel generates three constraint stories, one per reading. This story (republican_reading) focuses on consent-based legitimacy and its institutional instantiation through elections. The monarchical_reading story examines hereditary authority and divine-right legitimacy. The constitutional_hybrid_reading story addresses mixed authority (constitutional settlement). Each reading has its own ε value reflecting the empirical status of the legitimacy claim within its framework. The readings are linked via network.affects_constraints to enable contamination analysis: if the republican reading's consent mechanism degrades (theater_ratio rises, authenticity falls), pressure increases on hybrid and monarchical readings as alternative legitimacy sources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
