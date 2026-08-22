% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Ethno-National State Framework Persisting Past Its Coordination Function (Post-Zionist Reading)
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the post-zionist reading of the
 *   jewish_sovereignty_palestine kernel: it accepts that the Zionist project
 *   achieved a genuine, historically defensible coordination outcome
 *   (sovereign refuge from statelessness and persecution) but holds that the
 *   ethnic-national legal architecture built to secure that outcome — the Law
 *   of Return asymmetry, land allocation structures, and the 2018
 *   nation-state Basic Law — has outlived its original emergency function and
 *   now operates as an ongoing extraction and exclusion structure against
 *   Israeli Palestinians, the occupied populations of the West Bank and Gaza,
 *   and 1948 refugees. This is NOT the settler_colonial_reading (which denies
 *   the founding coordination function ever had legitimate title to the land
 *   regardless of intent) and NOT the liberal_nationalist_reading (which
 *   holds the ethnic-national framework remains a legitimate ongoing exercise
 *   of collective self-determination). The post-zionist claim is specifically
 *   genealogical and temporal: something that began as a coordination
 *   solution to a real emergency has calcified into a maintained extraction
 *   structure whose defenders increasingly rely on the founding narrative's
 *   continued emotional force rather than a live security justification that
 *   requires ethnic-national exclusivity as such.
 *
 * KEY AGENTS:
 *   - state_founding_institutions: agenda_setter, administers and could revise the ethnic-national legal architecture
 *   - jewish_israeli_citizens: primary beneficiary of automatic access to citizenship, land, and political voice
 *   - law_of_return_immigrants: beneficiary via unilateral immigration and settlement right
 *   - israeli_palestinian_citizens: payer, formal citizens facing systemic land and symbolic disadvantage
 *   - west_bank_palestinians: payer, live under military rule without citizenship or vote
 *   - gaza_palestinians: payer, blockaded and periodically bombarded territory outside citizenship framework
 *   - palestinian_refugees_1948: excluded, no return right and no seat in the legislative process that excludes them
 *   - regional_states_and_publics: observer, calibrate normalization against the arrangement's civic-equality compatibility
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.62).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.68).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Ethno-National State Framework Persisting Past Its Coordination Function (Post-Zionist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political philosophy / nationalism studies / postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, 'be2bdb0f-cea3-4327-93c9-1f96c219f79b').
narrative_ontology:cs_kernel_codification('be2bdb0f-cea3-4327-93c9-1f96c219f79b', distributed).
narrative_ontology:cs_authority_grounding('be2bdb0f-cea3-4327-93c9-1f96c219f79b', distributed).
narrative_ontology:cs_reading_relation('be2bdb0f-cea3-4327-93c9-1f96c219f79b', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('be2bdb0f-cea3-4327-93c9-1f96c219f79b', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('be2bdb0f-cea3-4327-93c9-1f96c219f79b', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('be2bdb0f-cea3-4327-93c9-1f96c219f79b', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_axiom('be2bdb0f-cea3-4327-93c9-1f96c219f79b', foundational, founding_coordination_legitimate_but_temporally_bound).
narrative_ontology:cs_axiom_status(founding_coordination_legitimate_but_temporally_bound, holdable).
narrative_ontology:cs_axiom_grounding('be2bdb0f-cea3-4327-93c9-1f96c219f79b', founding_coordination_legitimate_but_temporally_bound, empirically_contingent).
narrative_ontology:cs_axiom('be2bdb0f-cea3-4327-93c9-1f96c219f79b', foundational, ethnic_national_form_severable_from_security_function).
narrative_ontology:cs_axiom_status(ethnic_national_form_severable_from_security_function, holdable).
narrative_ontology:cs_axiom_grounding('be2bdb0f-cea3-4327-93c9-1f96c219f79b', ethnic_national_form_severable_from_security_function, instrumental).
narrative_ontology:cs_reference_frame('be2bdb0f-cea3-4327-93c9-1f96c219f79b', post_1948_emergency_refuge_state).
narrative_ontology:cs_drift_state('be2bdb0f-cea3-4327-93c9-1f96c219f79b', post_2018_nation_state_law_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('be2bdb0f-cea3-4327-93c9-1f96c219f79b', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_immigrants).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, state_founding_institutions).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_1948).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers land allocation, citizenship law, and national symbols through statutes (Law of Return, Absentee Property Law, national land authority arrangements) that were designed to secure Jewish demographic and territorial predominance during and after the state's founding emergency. Continues to enforce these arrangements as ordinary administration decades after the original security crisis that justified them, and can revise them through ordinary legislative process but has structural incentive not to.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, state_founding_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Hold automatic access to citizenship, land, housing, and political representation calibrated to a state whose institutions were built around their national collective. Many experience the arrangement simply as the ordinary, non-negotiable baseline of national life rather than as a system that allocates anything asymmetrically; they can leave the country if they choose but face no structural exclusion from its core institutions.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens, beneficiary,
    organized, biographical, mobile, national).

% Any Jewish person worldwide can immigrate and receive instant citizenship, land access, and settlement support under a statutory right that has no reciprocal analog for Palestinian refugees with generational ties to the same territory. This is the clearest transfer mechanism: land and residency rights move toward newcomers defined by ethnic-religious criteria.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, law_of_return_immigrants, beneficiary,
    moderate, biographical, mobile, global).

% Hold formal citizenship and voting rights but face systematic disadvantage in land allocation, budget distribution, and symbolic inclusion (national anthem, flag, the 2018 Basic Law defining self-determination as exclusively Jewish). Cannot access Jewish National Fund-administered land on equal terms in many areas; can vote and organize politically but the state's constitutional self-definition places a ceiling on what political mobilization can achieve without dismantling the ethnic-national framework itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, israeli_palestinian_citizens, payer,
    moderate, biographical, constrained, national).

% Live under military administration without citizenship or a vote in the state that controls their movement, land use, water access, and security regime, while adjacent settlements populated under the same founding framework receive full citizenship rights. No legal exit from the jurisdiction; political status is administered rather than self-determined.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians, payer,
    powerless, biographical, trapped, regional).

% Live under blockade and periodic military operation in a territory whose external boundaries, airspace, and coastline are controlled by the state without them holding its citizenship; the founding demographic logic (securing a Jewish-majority state) is frequently cited by both defenders and critics as a structural driver of the enclosure and recurring conflict.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, gaza_palestinians, payer,
    powerless, immediate, trapped, regional).

% Displaced in and after the 1948 war and their descendants, they and their claims are formally excluded from the state's citizenship and property-return frameworks; the Law of Return's asymmetric structure — automatic entry for Jewish immigrants, no reciprocal right of return for them — is the constraint's clearest transfer mechanism, yet their voice is absent from Israeli domestic legislative process entirely.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_1948, excluded,
    powerless, generational, trapped, regional).

% Arab states and publics negotiating normalization, security cooperation, or trade calibrate their positions partly against whether Israel's self-definition as an ethnic-national Jewish state is compatible with full regional integration and Palestinian civic equality; some governments proceed with normalization regardless, others make it conditional.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, regional_states_and_publics, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_israeli_citizens).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided Jewish communities fleeing persecution and genocide with a sovereign refuge and a state capable of self-defense and self-governance when no other polity would reliably guarantee either — a genuine collective-action solution to statelessness and vulnerability to majoritarian violence.
% TRANSFER_FUNCTION: Moves land, citizenship-track residency, and political voice toward Jewish citizens and eligible Jewish immigrants worldwide, and away from Palestinian citizens (relative disadvantage), West Bank and Gaza Palestinians (no citizenship, no vote, administered status), and 1948 refugees (no return right, no property restitution).
% ABSENT_VOICES: 1948 refugees and their descendants have no seat in the Israeli legislative process that defines citizenship and return rights; West Bank and Gaza Palestinians live under the state's control without a vote in it. Their objection — that a state built to end one people's statelessness was built partly atop another people's displacement, and that the arrangement has never been renegotiated with them as parties — is structurally excluded from the institution's own decision loop.
% DISAPPEARANCE_RATIONALE: If the ethnic-national legal architecture (Law of Return asymmetry, land allocation structures, the Jewish-nation-state Basic Law) were dismantled overnight in favor of civic equality for all residents and a resolved refugee question, land tenure, citizenship eligibility, political representation, and regional diplomatic relationships would all be renegotiated from the ground up — this is not a background natural fact but a constructed legal-institutional order with identifiable winners and losers.
% FOUNDING_PROBLEM: Jewish statelessness and vulnerability to majoritarian and genocidal violence in Europe and elsewhere, met by a movement for sovereign self-determination and a safe, self-governing homeland.
% FOUNDING_PROBLEM_CORROBORATION: Liberal Zionist and Israeli state historiography attest the founding problem (antisemitic persecution, need for refuge) remains live and justifies continued ethnic-preferential structures as a permanent safeguard. Independent sources outside the beneficiary group — UN human rights bodies, B'Tselem (an Israeli human rights organization reporting on Israel's own conduct), international legal scholarship on the 1967 occupation, and Palestinian civil society — attest that whatever the founding problem's original validity, the ongoing structure now functions primarily to maintain demographic and territorial control rather than to address an active existential threat of the founding kind; this post-zionist reading takes that latter corroboration as decisive for classifying the present-day arrangement, without denying the historical legitimacy of the original refuge-seeking motive.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-to-high (0.62) because the transfer mechanism (Law of Return asymmetry, land allocation, differential citizenship status across the territories the state controls) is real and substantial but coexists with a genuine, non-fabricated original coordination function (refuge from persecution) — this is a tangled rope, not a pure snare, from the post-zionist seat. Suppression (0.68) reflects active legal, administrative, and at times military enforcement of the boundary between who receives full civic status and who does not — military administration in the West Bank, blockade enforcement in Gaza, and legislative entrenchment (the 2018 Basic Law) of ethnic-national self-definition against civic-equality challenges. Theater ratio rose over time (0.15 in 1948 to 0.40-0.42 by the 2000s-2020s) as the founding emergency narrative increasingly does rhetorical work substituting for an active security justification specific to the ethnic exclusivity of land and citizenship rules, rather than merely to security itself.
 *
 * PERSPECTIVAL GAP:
 *   From the state_founding_institutions and jewish_israeli_citizens seats, the arrangement reads as the ordinary, non-negotiable baseline of national existence — often not experienced as an extraction structure at all, since its benefits are the water they swim in. From the israeli_palestinian_citizens, west_bank_palestinians, gaza_palestinians, and palestinian_refugees_1948 seats, the same legal architecture is directly experienced as differential and, in the occupied territories, coercively enforced. The engine's per-seat computation should reflect this asymmetry structurally rather than by narrative assertion.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish israeli citizens and Law of Return immigrants are coded as beneficiaries because the constraint's core transfer mechanisms (automatic citizenship, preferential land access, symbolic constitutional status) flow to them without their needing to seek exception; their exit options are mobile rather than constrained, consistent with low derived directionality. Israeli Palestinian citizens are constrained rather than trapped — they hold formal citizenship and voting rights, but the ceiling imposed by the state's ethnic-national self-definition limits what electoral mobilization alone can achieve. West Bank and Gaza Palestinians are trapped: they live under the state's effective control without its citizenship, with no independent exit route, which is why their directionality sits at the extraction end regardless of formal legal categorization. Palestinian_refugees_1948 are excluded rather than payer in the stakeholder role sense because their relationship to the constraint is defined by non-inclusion in its citizenship and property frameworks rather than by ongoing participation in them, even though the Law of Return asymmetry is the constraint's single clearest transfer mechanism against their claims.
 *
 * MANDATROPHY ANALYSIS:
 *   The post-zionist reading's central move is a mandatrophy claim: the founding problem (Jewish statelessness and persecution) was real and the coordination response (sovereign refuge) was a legitimate solution to it, but the specific ethnic-national legal architecture built to secure that response has persisted past the point where it required the exclusionary form it still takes. This resists two collapse failures: treating the entire project as pure extraction from inception (which would erase the founding coordination function and the historical persecution that motivated it — the settler_colonial_reading's stronger claim, which this reading does not adopt) and treating the present architecture as still fully justified by an unchanged founding emergency (the liberal_nationalist_reading, which this reading contests as no longer matching the state's actual current security situation). The tangled_rope classification — genuine coordination function plus asymmetric ongoing extraction requiring active enforcement — is precisely the category built to hold both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_legitimacy_vs_ongoing_structure,
    'Does establishing that the founding coordination function (refuge from persecution) was genuine and historically justified settle anything about whether the present-day ethnic-national legal architecture remains justified, or are these separable questions that this reading has to keep analytically distinct?',
    'Comparative analysis of how other post-emergency national projects (e.g., post-war European nation-states, post-independence settler and non-settler states) have or have not revised founding exclusionary provisions once the founding emergency receded; direct empirical assessment of whether Israel''s current external security situation still requires ethnic-exclusive citizenship and land law as opposed to non-ethnic security measures.',
    'If the two questions are genuinely separable and current security needs do not require ethnic exclusivity, the tangled_rope classification (legitimate founding function, current extraction) is well-supported. If they are not separable — if the ethnic-national form remains a functional requirement of the state''s security architecture even now — the constraint moves structurally closer to the liberal_nationalist_reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_legitimacy_vs_ongoing_structure, conceptual, 'Whether founding legitimacy and present-day structural justification are separable questions.').

omega_variable(
    reading_selection_and_referent,
    'This story is one of five declared readings of the jewish_sovereignty_palestine kernel (cultural_zionist, liberal_nationalist, post_zionist, religious_zionist, settler_colonial). Each reading authors a different beneficiary/victim structure and a different epsilon for what is nominally ''the same'' arrangement. What signals guided selection of the post_zionist framing here rather than, say, the settler_colonial framing, given that both readings identify the same victim populations?',
    'The distinguishing signal is the treatment of the founding function: post_zionist accepts the founding coordination problem (statelessness, persecution) as real and grants the original refuge-seeking project historical legitimacy, then locates the extraction in the persistence of the ethnic-national legal form past its emergency function. Settler_colonial denies that framing has structural priority at all, treating displacement as constitutive of the project from inception regardless of the founding population''s motivating vulnerability. The two readings could be distinguished empirically only by disputed historical questions (extent of pre-1948 land purchase vs. expropriation, the causal role of the 1948 war''s expulsions and departures) that this story does not adjudicate.',
    'If the settler_colonial framing is adopted instead, epsilon rises further (approaching snare) because the founding coordination function itself is denied rather than granted-then-critiqued; the tangled_rope classification here depends on accepting a genuine coordination function existed at founding, which is exactly what the sibling reading disputes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_referent, conceptual, 'Documents why this story selected the post-zionist framing over the settler-colonial framing given overlapping victim sets.').

omega_variable(
    de_zionization_feasibility,
    'Is a de-Zionized, fully civic-egalitarian state structure (the post-zionist reading''s implicit endorsed alternative) achievable through internal legal reform, or does it require external constitutional imposition or a negotiated two-state/one-state settlement that the current institutional structure has no internal mechanism to produce?',
    'Track actual legislative and judicial reform efforts (e.g., Israeli Supreme Court rulings on the nation-state law, civil society campaigns for equal citizenship) against their success rate and the political costs imposed on their proponents.',
    'If internal reform is structurally foreclosed (e.g., the nation-state Basic Law is entrenched against ordinary legislative reversal), this pushes the classification toward snare, since the extraction becomes not just ongoing but self-insulating against removal by the payer seats'' own political action. If internal reform paths remain genuinely open, tangled_rope is a more stable classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(de_zionization_feasibility, empirical, 'Whether civic-egalitarian reform is achievable within the existing institutional structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(jewi_tr_t1948, observed).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1967, 0.2).
narrative_ontology:measurement_basis(jewi_tr_t1967, observed).
narrative_ontology:measurement(jewi_tr_t1993, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement_basis(jewi_tr_t1993, observed).
narrative_ontology:measurement(jewi_tr_t2000, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement_basis(jewi_tr_t2000, observed).
narrative_ontology:measurement(jewi_tr_t2018, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2018, 0.42).
narrative_ontology:measurement_basis(jewi_tr_t2018, observed).
narrative_ontology:measurement(jewi_tr_t2024, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 2024, 0.4).
narrative_ontology:measurement_basis(jewi_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement_basis(jewi_be_t1948, observed).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement_basis(jewi_be_t1967, observed).
narrative_ontology:measurement(jewi_be_t1993, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement_basis(jewi_be_t1993, observed).
narrative_ontology:measurement(jewi_be_t2000, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(jewi_be_t2000, observed).
narrative_ontology:measurement(jewi_be_t2018, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement_basis(jewi_be_t2018, observed).
narrative_ontology:measurement(jewi_be_t2024, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 2024, 0.62).
narrative_ontology:measurement_basis(jewi_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement_basis(jewi_su_t1948, observed).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement_basis(jewi_su_t1967, observed).
narrative_ontology:measurement(jewi_su_t1993, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement_basis(jewi_su_t1993, observed).
narrative_ontology:measurement(jewi_su_t2000, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement_basis(jewi_su_t2000, observed).
narrative_ontology:measurement(jewi_su_t2018, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement_basis(jewi_su_t2018, observed).
narrative_ontology:measurement(jewi_su_t2024, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 2024, 0.68).
narrative_ontology:measurement_basis(jewi_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the jewish_sovereignty_palestine kernel. Each reading authors a distinct epsilon, beneficiary/victim structure, and classification for what natural language flattens into a single label ('the Zionist project' / 'Israeli statehood'). The post_zionist_reading accepts the founding coordination function's historical legitimacy while classifying the present-day ethnic-national legal architecture as tangled_rope (coordination-plus-extraction, requiring active enforcement). The settler_colonial_reading denies the founding coordination function had legitimate title from inception (likely classifying closer to snare). The liberal_nationalist_reading treats the current ethnic-national structure as a still-live, legitimate exercise of collective self-determination (likely classifying closer to rope or scaffold with the security emergency as ongoing justification). The religious_zionist_reading grounds the claim theologically, outside empirical contest entirely (a distinct cs_structure authority_grounding). The cultural_zionist_reading denies that political sovereignty and demographic control were ever structurally necessary, implying a much lower epsilon for a hypothetical cultural-center-only arrangement. Per the ε-invariance principle, these are five separate constraints sharing a kernel, not one constraint measured five ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
