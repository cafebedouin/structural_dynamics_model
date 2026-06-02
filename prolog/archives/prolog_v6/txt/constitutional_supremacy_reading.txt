% ============================================================================
% CONSTRAINT STORY: constitutional_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_supremacy_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_supremacy_reading
 *   human_readable: Constitutional Supremacy in Family Law Authority (State Monopoly Reading)
 *   domain: constitutional_law/legal_pluralism/family_law
 *
 * SUMMARY:
 *   This constraint models family law authority under the constitutional
 *   supremacy reading of the family_law_authority kernel. The reading
 *   instantiates a specific architectural choice: the constitutional state
 *   claims exclusive legal authority over marriage, divorce, inheritance, and
 *   child custody, relegating religious and communal family law to the
 *   private sphere without legal recognition or enforcement. This constraint
 *   is one structural instantiation of a contested kernel — the fundamental
 *   question of who legitimately governs family relations. The constitutional
 *   supremacy reading assumes state monopoly as the correct answer, grounded
 *   in individual rights protection, rule of law, and secular governance.
 *   Sibling readings (communal autonomy and hybrid accommodation) would
 *   instantiate different constraints with different beneficiary/victim
 *   structures, different extractiveness values, and different perspectival
 *   gaps. This story generates ONLY the supremacy reading: a clean,
 *   ε-invariant constraint at 0.28 extractiveness, showing both genuine
 *   coordination function (state enforcement of property and consent rights)
 *   and asymmetric extraction (suppression of communal authority). The
 *   theater ratio (0.58) reflects that much constitutional family law
 *   narrative performs ideological work beyond coordinating function — the
 *   claim that state monopoly is 'necessary' or 'natural' naturalizes what is
 *   actually a contingent institutional choice visible in comparative
 *   perspective.
 *
 * KEY AGENTS:
 *   - Constitutional State: Primary beneficiary (institutional/arbitrage) — monopolizes legal authority over family matters; no external constraint on state jurisdiction
 *   - Secular Citizens: Secondary beneficiary (powerful/mobile) — protected from religious override by state rules; no suppression experienced
 *   - Religious Minority Communities: Primary victim (powerless/identity_locked) — lose communal authority over family law; cannot exit due to identity fusion with tradition
 *   - Religious Individuals: Secondary victim (moderate/constrained) — face dual obligation (state law and communal expectation); high conformity cost
 *   - Religious Institutions: Tertiary victim (institutional/constrained) — lose legal authority historically held; cannot formalize or enforce communal family law
 *   - Liberal Constitutional Framework: Institutional actor (institutional/arbitrage) — sees supremacy as natural necessity; theatrical maintenance of ideology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_supremacy_reading, 0.28).
domain_priors:suppression_score(constitutional_supremacy_reading, 0.52).
domain_priors:theater_ratio(constitutional_supremacy_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_supremacy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(constitutional_supremacy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_supremacy_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_supremacy_reading, "Constitutional Supremacy in Family Law Authority (State Monopoly Reading)").
narrative_ontology:topic_domain(constitutional_supremacy_reading, "constitutional_law/legal_pluralism/family_law").

domain_priors:requires_active_enforcement(constitutional_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(constitutional_supremacy_reading, fixed_text).
narrative_ontology:cs_authority_grounding(constitutional_supremacy_reading, expertise).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_supremacy_reading, constitutional_state).
narrative_ontology:constraint_beneficiary(constitutional_supremacy_reading, secular_citizens).
narrative_ontology:constraint_victim(constitutional_supremacy_reading, religious_minority_communities).
narrative_ontology:constraint_victim(constitutional_supremacy_reading, communal_autonomy_practices).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RELIGIOUS MINORITY COMMUNITY (SNARE) — Structurally mobile (could migrate, could abandon religious practice) but identity-locked through religious commitment, family structure constituted through communal law, and social bonds embedded in the tradition. The constraint extracts conformity to secular state rules for marriage, divorce, inheritance, and child custody despite these being matters the community views as governed by religious authority. High suppression: state enforcement machinery (courts, child protective services, inheritance law) overrides communal norms. Maximum experienced extraction — the community pays the conformity cost while losing authority over their own family structures.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: RELIGIOUS INDIVIDUAL WITHIN THE MINORITY COMMUNITY (TANGLED ROPE) — Faces genuine coordination benefit (state family law provides inheritance stability, child protection, property rights enforcement) alongside extraction (cannot formally recognize religious marriage, faces custody battles if the state views religious practice as conflicting with child welfare, must navigate dual legal systems). Exit options constrained — leaving the community incurs social cost and loss of religious identity; accepting state law only incurs legal cost. Mixed experience: some benefits from state infrastructure, significant extraction through loss of communal authority.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL STATE (ROPE) — Experiences the constraint as pure coordination: uniform family law is necessary to solve collective action problems (property rights, child protection, spousal consent enforceability). The state benefits from monopoly on legal authority — no competing family law regime can undermine state enforcement power. Net beneficiary: extraction flows toward the state as institutional actor. The state perceives this as legitimate coordination, not extraction, because the coordination problems are real.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SECULAR CITIZEN (ROPE) — Benefits from uniform family law applied without religious override. Experiences the constraint as pure coordination: state authority prevents religious minorities from using family law to control women (e.g., forced marriage), enforces consent and property rights, and provides uniform custody standards. The secular citizen is a net beneficiary with no suppression — they experience the constraint as legitimate state authority, not extraction.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RELIGIOUS INSTITUTION / COMMUNAL AUTHORITY (TANGLED ROPE) — Experiences the constraint as extraction: loss of legal authority over marriage, divorce, inheritance (historically performed by religious courts or councils). Also experiences coordination benefit: state enforcement of property rights and child protection outside the community may reduce internal coercion. But the primary experience is suppression of institutional authority — legal monopoly prevents the religious institution from enforcing its own family law regime. Exit options are constrained: the institution cannot migrate, cannot abandon its traditional authority claims without losing legitimacy, must negotiate with state within existing constitutional framework.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LIBERAL CONSTITUTIONAL FRAMEWORK / COMPARATIVE INSTITUTIONAL VIEW (PITON) — From a long-term and global comparative lens, this constraint is substantially theatrical: the claim that constitutional supremacy is 'necessary' for family law coordination is historically contingent. Many jurisdictions (India, Israel, Malaysia, Pakistan, Nigeria) maintain dual or plural family law systems where religious authority coexists with state authority, often with stable outcomes and lower theater than the all-encompassing state monopoly. The supremacy claim persists through institutional inertia and liberal ideology rather than empirical demonstration of functional necessity. Theater ratio high (0.58) because the constitutional narrative performs an ideological function — naturalizing state authority as inevitable — beyond its coordinating function.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / RIGHTS-BASED NATURAL LAW (MOUNTAIN) — From a universalizing analytical position, individual rights (consent, property, bodily autonomy, freedom of association) represent immutable human requirements that any family law system must protect. From this view, constitutional supremacy is not a contingent institutional choice but a necessary floor — religious authority cannot override individual rights without violating the natural law of human dignity. However, this classification risks false summit: the analytical view naturalizes what is actually a contestable liberal commitment (individual rights as supreme) against alternative framings (communal autonomy, religious authority as legitimate pluralist claim). The engine will detect this as a false summit candidate.
constraint_indexing:constraint_classification(constitutional_supremacy_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_supremacy_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_supremacy_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_supremacy_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_supremacy_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low. The state genuinely performs coordinating functions — enforcing property rights, protecting consent and bodily autonomy, preventing coerced marriage. The constraint is not pure extraction because the coordination problems are real. However, extractiveness rises from 0.15 to 0.28 over the historical interval (0–100) as state enforcement strengthens and communal alternatives weaken. The base value reflects the measurable asymmetry: secular citizens and the state experience pure coordination (Rope), while religious minorities experience simultaneous coordination and extraction (Tangled Rope). Suppression (0.52): Moderate-high. Significant barriers to exercising communal family law authority include state legal monopoly (no parallel enforcement system), child protective services (state can override parental/communal custody decisions), inheritance law (state rules supersede religious rules), and social pressure (state norms become default expectation). Suppression is not total because informal parallel enforcement persists (some communities maintain unofficial religious marriage, divorce, and inheritance practices). Theater ratio (0.58): Moderate. The constitutional narrative performs ideological work — the claim that state monopoly is 'necessary' naturalizes what comparative law shows to be contingent. The theater increases over the interval as the state's claim to necessity becomes more elaborate and the institutional inertia deepens. However, the theater is not as high as in pure Piton (where function has atrophied) because genuine coordination function persists. The theater represents the gap between stated justification (necessity, natural law, inevitable progress) and actual structure (contingent choice, reversible in principle).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival gap driven by directionality. The constitutional state and secular citizens classify the constraint as Rope (pure coordination) because they experience low suppression and direct benefits. Religious minorities classify it as Snare (pure extraction) because they experience maximum suppression and loss of communal authority, with no exit available (identity_locked). Religious individuals classify it as Tangled Rope (mixed coordination and extraction) because they benefit from state-enforced property rights but lose communal authority and face dual obligations. The piton perspective (institutional/civilizational) reveals that much constitutional supremacy rhetoric is theatrical — the claim to empirical necessity is not sustained by comparative law, which shows plural and dual family law systems achieving stable, comparable outcomes. The analytical/mountain perspective risks naturalizing this contingent choice as immutable human rights requirement, but the false summit detector identifies this as a naturalization of liberal institutional choice rather than a genuine natural law. The gap reveals that 'family law authority' is contested at the level of how legitimacy is grounded: liberal individual rights versus communal autonomy versus hybrid negotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position relative to this constraint. The state and secular citizens are beneficiaries with low experienced extraction — the constraint subsidizes their preferred legal regime, so their d ≈ 0.10–0.20. Religious minorities are victims with identity-locked exit — they cannot leave without abandoning their identity, so their d ≈ 0.85–0.95, producing high experienced extraction chi via the sigmoid f(d). Religious institutions are institutional victims with constrained exit (cannot migrate, cannot delegate, must engage state within existing framework) — d ≈ 0.65, producing moderate-high chi. The perspectival gap between beneficiary (Rope) and victim (Snare) perspectives reflects this directionality variance. The engine derives d from beneficiary/victim declarations plus exit options; no explicit override needed — the structural data is sufficient.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Which kernel reading is this constraint instantiating?',
    'Authorial declaration: This JSON instantiates the ''constitutional_supremacy_reading'' of the family_law_authority kernel. The sibling readings are: communal_autonomy_reading (religious authority is legitimate co-jurisdiction within constitutional limits) and hybrid_accommodation_reading (state and religious authority coexist with negotiated boundaries).',
    'This reading assumes state monopoly on family law authority. The communal_autonomy_reading would shift beneficiary/victim declarations (religious institution becomes beneficiary, state becomes partial victim). The hybrid_accommodation_reading would show lower extractiveness and higher coordination value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Kernel: family_law_authority. Reading: constitutional_supremacy_reading. Sibling readings: communal_autonomy_reading, hybrid_accommodation_reading.').

omega_variable(
    liberal_ideology_vs_empirical_necessity,
    'Is constitutional supremacy in family law an empirical necessity for coordinating family rights, or a contingent institutional choice justified post-hoc by liberal ideology?',
    'Comparative analysis of plural and dual family law jurisdictions (India, Israel, Malaysia, Nigeria, Catalonia, Scotland before Union): stability of outcomes, rates of family-law abuse, enforceability of contracts, child protection, property rights protection. If plural systems achieve similar outcomes with lower suppression, constitutional supremacy is contingent, not necessary.',
    'If necessity: mountain classification at analytical level is correct, and suppression is justified as the cost of coordination. If contingent: mountain is a false summit, suppression is revealed as extractive overhead, and alternative plural systems become visible as Rope or lower-extractiveness Tangled Rope alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liberal_ideology_vs_empirical_necessity, empirical, 'Empirical necessity vs. ideological contingency of constitutional supremacy').

omega_variable(
    religious_authority_legitimacy,
    'Do religious minorities accept constitutional state authority over family law as legitimate, or do they perceive it as external imposition?',
    'Survey and historical data: rates of compliance with state family law among religious communities; rates of informal parallel enforcement of religious law; political mobilization and resistance; generational drift in acceptance. High acceptance + low resistance suggests legitimacy. High resistance + persistent parallel systems suggests perceived imposition.',
    'If perceived as legitimate: suppression value lower than (0.52) suggests; classification may shift toward Rope from minority perspective. If perceived as imposition: identity_locked exit option and high suppression justified; Snare classification correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(religious_authority_legitimacy, empirical, 'Perceived legitimacy of constitutional state authority by religious minorities').

omega_variable(
    women_protection_vs_communal_autonomy,
    'Does constitutional supremacy protect women from abuse within religious family law systems, or do informal plural systems achieve similar protection with lower suppression?',
    'Comparative data on women''s rights violations, domestic violence rates, forced marriage prevalence, divorce initiation rights in constitutional supremacy vs. plural family law jurisdictions. Correlation analysis controlling for enforcement capacity and resource allocation.',
    'If constitutional supremacy has measurably better outcomes: legitimacy of suppression increases; Tangled Rope classification emphasizes coordination function. If outcomes similar or worse due to underground enforcement: constitutional supremacy is revealed as extractive theater; suppression unjustified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(women_protection_vs_communal_autonomy, empirical, 'Comparative women''s protection under constitutional supremacy vs. plural family law systems').

omega_variable(
    false_summit_natural_law_candidate,
    'Is this constraint a genuine natural law (immutable human rights requirement) or a naturalized institutional choice?',
    'Cross-cultural validity test: Do the rights claims (consent, bodily autonomy, property rights, child welfare) translate across cultural contexts without contradiction? Or do they presuppose liberal individualism? If translation requires reframing or enforces liberal norms against genuine alternative frameworks (relational autonomy, communal obligation), the ''natural law'' status is contingent.',
    'If universal: mountain classification is correct. If contingent on liberal framework: false summit detected; constraint reclassifies to Tangled Rope at analytical level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'False summit candidate: Natural law rights vs. naturalized liberal institutional choice').

omega_variable(
    historical_contingency_of_state_monopoly,
    'Is state monopoly on family law authority a recent development tied to nation-state formation, or a necessary feature of any functional legal system?',
    'Historical analysis: family law authority in pre-nation-state systems (Islamic law, Hindu law, Jewish law, canon law, customary law) and in contemporary non-Western states. Pattern analysis: does state monopoly emerge from deliberate policy or from colonial imposition? Do alternative arrangements persist through deliberate choice or through state failure to enforce monopoly?',
    'If recent and contingent: suppression is political choice, not natural necessity. If universal and necessary: suppression is justified coordinating cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_contingency_of_state_monopoly, empirical, 'Historical contingency of state family law monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_supremacy_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_supremacy_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cons_tr_t50, constitutional_supremacy_reading, theater_ratio, 50, 0.5).
narrative_ontology:measurement(cons_tr_t100, constitutional_supremacy_reading, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_supremacy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cons_be_t50, constitutional_supremacy_reading, base_extractiveness, 50, 0.22).
narrative_ontology:measurement(cons_be_t100, constitutional_supremacy_reading, base_extractiveness, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_supremacy_reading, resource_allocation).
narrative_ontology:affects_constraint(constitutional_supremacy_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(constitutional_supremacy_reading, hybrid_accommodation_reading).

% DUAL FORMULATION NOTE:
% The family_law_authority kernel admits three structurally distinct readings with different ε values, beneficiary/victim structures, and perspectival classifications. This story (constitutional_supremacy_reading, ε≈0.28) is the state-monopoly reading. Sibling stories model communal autonomy (ε≈0.15–0.20, lower extraction, religious institution as beneficiary) and hybrid accommodation (ε≈0.18–0.24, negotiated boundaries, both state and communities benefit). The kernel question persists across readings; the constraint structure changes with the reading instantiated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
