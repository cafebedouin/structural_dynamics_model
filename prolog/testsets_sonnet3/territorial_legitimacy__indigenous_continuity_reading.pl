% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy — Indigenous Continuity / Anti-Colonial Self-Determination Reading
 *   domain: political/international_law/territorial_sovereignty
 *
 * SUMMARY:
 *   Under the indigenous-continuity reading, the modern Israeli state's
 *   territorial control over historic Palestine is read as the continuation
 *   of a 1948 dispossession event rather than the exercise of a legitimately
 *   founded sovereignty. The reading centers Palestinian refugees' right of
 *   return, treats the settlement enterprise as an extension of the same
 *   dispossession logic into the West Bank and East Jerusalem, and treats
 *   international recognition instruments (UNGA 181, subsequent state
 *   recognition) as insufficient to cure a founding harm that was never
 *   remedied. This reading is authored as a snare: the coordination story
 *   offered by the Israeli state (nation-building, refuge for a persecuted
 *   people, security architecture) is, by this reading's own lights, cover
 *   for an arrangement whose persistence depends on active suppression —
 *   permit regimes, military administration, demographic engineering through
 *   citizenship and residency law — and on the structural exclusion of an
 *   identifiable victim population (1948 refugees and their descendants,
 *   Palestinians under occupation, and second-class Palestinian citizens)
 *   from remedy.
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: institutional agenda-setter administering the territory, arbitrage-grade exit — collects sovereignty and security benefits from the founding arrangement
 *   - settlement_enterprise_institutions: organized beneficiary collecting land and resource allocation directly from continued territorial control
 *   - palestinian_1948_refugees_and_descendants: powerless, trapped, civilizational time horizon — bear the central and unremedied cost this reading is built around
 *   - palestinians_under_occupation: powerless, trapped, immediate daily cost under permit and military administration regimes
 *   - palestinian_citizens_of_israel: moderate power, constrained exit — formal citizenship without capacity to alter the founding premise
 *   - international_legal_scholars: analytical observer assessing competing legal bases without power to adjudicate outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.87).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.88).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.87).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy — Indigenous Continuity / Anti-Colonial Self-Determination Reading").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political/international_law/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, 'bb69ba75-411a-4317-b087-5f2893e57db8').
narrative_ontology:cs_kernel_codification('bb69ba75-411a-4317-b087-5f2893e57db8', distributed).
narrative_ontology:cs_authority_grounding('bb69ba75-411a-4317-b087-5f2893e57db8', distributed).
narrative_ontology:cs_reading_relation('bb69ba75-411a-4317-b087-5f2893e57db8', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('bb69ba75-411a-4317-b087-5f2893e57db8', territorial_legitimacy__security_necessity_reading, influences).
narrative_ontology:cs_axiom('bb69ba75-411a-4317-b087-5f2893e57db8', foundational, indigenous_continuity_grounds_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_continuity_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('bb69ba75-411a-4317-b087-5f2893e57db8', indigenous_continuity_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('bb69ba75-411a-4317-b087-5f2893e57db8', foundational, founding_partition_instrument_void_by_dispossession).
narrative_ontology:cs_axiom_status(founding_partition_instrument_void_by_dispossession, holdable).
narrative_ontology:cs_axiom_grounding('bb69ba75-411a-4317-b087-5f2893e57db8', founding_partition_instrument_void_by_dispossession, conventional).
narrative_ontology:cs_axiom('bb69ba75-411a-4317-b087-5f2893e57db8', secondary, right_of_return_not_extinguished_by_time).
narrative_ontology:cs_axiom_status(right_of_return_not_extinguished_by_time, holdable).
narrative_ontology:cs_axiom_grounding('bb69ba75-411a-4317-b087-5f2893e57db8', right_of_return_not_extinguished_by_time, deontological).
narrative_ontology:cs_reference_frame('bb69ba75-411a-4317-b087-5f2893e57db8', pre_1948_indigenous_habitation_pattern).
narrative_ontology:cs_drift_state('bb69ba75-411a-4317-b087-5f2893e57db8', contemporary_post_oslo_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('bb69ba75-411a-4317-b087-5f2893e57db8', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, settlement_enterprise_institutions).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_1948_refugees_and_descendants).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinians_under_occupation).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, settler_colonial_framework_validity).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, indigenous_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territory as sovereign, controls the population registry, movement permits, land allocation, and citizenship law across historic Palestine. From this reading's frame, its authority rests on a 1948 founding event this reading characterizes as the Nakba — mass displacement and depopulation, not a lawful state-founding partition — and its continued administration depends on maintaining that founding as settled rather than contested.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus, beneficiary).

% Government ministries, settlement authorities, and land administration bodies that allocate land in the West Bank and East Jerusalem, drawing on state resources and legal frameworks that this reading holds are extensions of the same 1948 dispossession logic. They collect land, housing, and resource benefits directly from continued territorial control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, settlement_enterprise_institutions, beneficiary,
    organized, generational, arbitrage, national).

% Displaced in 1948 and their multi-generational descendants, dispersed across refugee camps in neighboring states, the diaspora, and areas under Palestinian Authority or Israeli control. Under this reading their claim to return and to property in what is now Israel is not extinguished by time or by a partition instrument they never accepted; statelessness and camp confinement persist across generations with no legal path back.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_1948_refugees_and_descendants, payer,
    powerless, civilizational, trapped, regional).

% Residents of the West Bank and Gaza living under military administration, checkpoints, permit regimes, and settlement expansion. Under this reading they are the present-tense continuation of the population the 1948 events sought to remove or subordinate; daily movement, land use, and political voice are constrained by an apparatus this reading treats as the ongoing instrument of the founding dispossession.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinians_under_occupation, payer,
    powerless, biographical, trapped, local).

% Hold formal citizenship but, under this reading, live inside a state structure whose basic laws privilege Jewish national self-determination in ways that structurally subordinate their claim to indigenous continuity on the same land; they vote and litigate but cannot legislate the founding premise away.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel, payer,
    moderate, generational, constrained, national).

% Issue resolutions (e.g., UNGA 194 on return, ICJ advisory findings) that this reading treats as corroborating the indigenous-continuity claim, but have no enforcement capacity over the territory; their findings are cited by advocates of this reading but carry no binding force absent state compliance, leaving them structurally present in argument but absent from actual disposition of land and return.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, un_and_international_legal_bodies, excluded,
    institutional, generational, analytical, global).

% Governments, solidarity movements, and diaspora Palestinian organizations that advance the indigenous-continuity and right-of-return claims internationally but have no seat in the bilateral or multilateral negotiating structures that actually determine territorial disposition.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, third_party_states_and_diaspora_advocates, excluded,
    organized, generational, analytical, global).

% Assess the competing legal bases — partition instrument, occupation law, self-determination doctrine, settler-colonial analysis — without the power to adjudicate a binding outcome; their scholarship shapes but does not resolve which reading of the kernel governs actual territorial control.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement's only genuine coordination function is exercised BY Palestinians among themselves and with international solidarity networks to preserve historical memory, refugee registration (via UNRWA), and a unified claim to return — a coordination problem imposed on the dispossessed, not a coordination the territorial arrangement itself provides. The Israeli state apparatus's administration of the territory is not read by this constraint as coordination; it is read as the mechanism that continues the founding dispossession.
% TRANSFER_FUNCTION: Moves land, residency rights, water and resource access, and political self-determination from the indigenous Palestinian population (1948 refugees, their descendants, and those remaining under occupation or as second-class citizens) to the Israeli state and the settlement enterprise, formalized through population registries, land expropriation law, permit regimes, and military administration.
% ABSENT_VOICES: 1948 refugees and their descendants are the paradigm absent voice: their claim is the structural center of this reading yet they hold no seat in any negotiating or administrative body that currently disposes of the land or citizenship they claim. UN resolutions affirming return exist on paper but have never been the basis of an enforced remedy.
% DISAPPEARANCE_RATIONALE: If the current territorial and citizenship arrangement were dissolved and this reading's premise enacted — a single sovereign entity over historic Palestine grounded in indigenous continuity, with a right of return implemented — the demographic, political, and property structure of the entire territory would be reorganized: current Israeli citizenship law, the settlement enterprise, and refugee statelessness would all cease to exist in their current form.
% FOUNDING_PROBLEM: The founding event this reading identifies is the 1948 war and mass displacement of Palestinian Arabs (the Nakba), which this reading holds was not a lawful transfer of sovereignty under the UN partition plan but a campaign of depopulation that produced a refugee population whose claims were never resolved.
% FOUNDING_PROBLEM_CORROBORATION: Some corroboration exists outside the parties directly benefiting from the current arrangement: UN General Assembly Resolution 194 (1948) affirms a right of return for refugees wishing to live at peace with their neighbors, and Israeli 'New Historian' scholars (working from declassified Israeli military archives, not Palestinian sources) have independently documented expulsions and depopulation operations in 1948 consistent with this reading's founding-problem narrative. The Israeli state apparatus and settlement institutions dispute both the characterization of the founding event and its continued relevance, treating the matter as legally and politically settled by 1948 and subsequent conflicts.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.87, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.87 by 2024) because, under this reading, the transfer from the indigenous population to the state and settlement apparatus is total and unremedied across three generations: land, residency, and political voice all move in one direction with no restitution mechanism. Suppression is authored even higher (0.88) because the reading holds that active machinery — military administration, permit regimes, demolition orders, citizenship-and-residency law — is required continuously to prevent return and to manage the subordinated population; this is not a constraint that would persist on its own accord if the suppressive apparatus were withdrawn. Theater ratio is moderate (0.42) reflecting this reading's view that some genuine security and administrative function exists alongside a growing performative layer (peace-process diplomacy, humanitarian carve-outs) that does not alter the underlying extraction trajectory. Accessibility collapse is authored LOWER than a pure natural-law constraint (0.35) because, under this reading, alternatives are not conceptually foreclosed — return, restitution, and a single-state or altered-sovereignty arrangement remain politically imaginable and are actively argued for; what collapses is not the alternative's coherence but its practical achievability against the enforcement apparatus. Resistance is authored very high (0.9) because this reading holds that the arrangement is met with continuous, organized resistance across generations (uprisings, diplomatic campaigns, refugee mobilization, international solidarity movements) — the opposite of a settled natural fact.
 *
 * DIRECTIONALITY LOGIC:
 *   The Israeli state apparatus and the settlement enterprise institutions are the structural beneficiaries under this reading: their exit options are arbitrage-grade (they set and can revise the rules) and the constraint transfers land, residency, and political voice toward them. The 1948 refugees and their descendants sit at the extreme target end — powerless, trapped across generations, civilizational time horizon with no remedy pathway; this reading requires their d be authored near the full-target end because the arrangement's entire claimed founding problem (Nakba, not partition) is defined by their unremedied displacement. Palestinians under occupation are similarly near-full-target but with an immediate rather than civilizational time horizon. Palestinian citizens of Israel are authored with moderate power and constrained (not trapped) exit, reflecting genuine legal citizenship that nonetheless does not reach the founding premise — this differentiates them from the refugees and the occupied population without treating their situation as a beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists reduction to either 'timeless coordination' or 'pure ancient grievance with no present function' by keeping the founding_problem_status explicitly contested: the reading holds the founding problem (the Nakba, and refugees' unresolved claim) is LIVE, not dead, because the population it displaced still exists, still lacks remedy, and the state apparatus's core legitimating claims still depend on treating 1948 as settled. Classifying this as snare rather than tangled_rope reflects that, by this reading's own lights, the coordination story (nation-building, refuge, security) is COVER rather than a genuine independent coordination function coexisting with extraction — the reading does not concede that Israeli state formation solved a real, separable coordination problem for Palestinians; it holds the entire arrangement is organized around their displacement. A tangled_rope classification would concede more coordination legitimacy to the founding arrangement than this reading holds is warranted; that concession belongs to the sibling partition_reading and security_necessity_reading constraints, not here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indigenous_continuity_of_territorial_legitimacy,
    'This constraint instantiates the indigenous_continuity_reading of the territorial_legitimacy kernel. Sibling readings (partition_reading: legitimacy via UN Resolution 181 and international state recognition; security_necessity_reading: legitimacy via 1967-plus strategic depth and defensive necessity) are separate constraints with their own ε, beneficiary/victim structure, and classification. What would a sibling reading change structurally, and where is the disagreement located?',
    'The disagreement is located at the characterization of the founding event (1948 as Nakba/dispossession vs. as lawful partition and state formation vs. as a security-driven territorial settlement subsequently entrenched by 1967 and later conflicts) and at whether the current arrangement''s coordination story is cover for extraction (this reading) or a genuine, separable coordination achievement (partition_reading) or a necessary defensive posture (security_necessity_reading). No empirical resolution mechanism fully adjudicates this — it depends on which normative framework (self-determination/anti-colonial doctrine, international legal instrument validity, state security doctrine) is taken as primary, which is itself a contested political and philosophical commitment.',
    'If the partition_reading''s premise (legitimate international legal founding via UNGA 181, cured by subsequent state recognition) is adopted instead, ε and the beneficiary/victim structure would look substantially different — extraction would be authored much lower and the coordination function would be treated as real rather than as cover, likely yielding a tangled_rope or even rope classification for the founding arrangement''s core.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indigenous_continuity_of_territorial_legitimacy, conceptual, 'Committer-frame structure: this reading vs. its siblings within the territorial_legitimacy kernel.').

omega_variable(
    founding_event_characterization,
    'Is the 1948 event best characterized, for purposes of legitimacy assessment, as the Nakba (a campaign of mass displacement invalidating claims to lawful founding) or as a partition and war whose outcome, however costly, was subsequently legitimated through international recognition and decades of state practice?',
    'Historical and archival research (already substantially advanced by Israeli ''New Historian'' scholarship using declassified military archives) can establish factual matters — the extent and intentionality of expulsions, the sequence of events relative to Arab state intervention — but the NORMATIVE conclusion (whether such events void a state''s legitimacy claim decades later) is not resolvable by additional facts alone; it depends on which legitimacy doctrine is applied.',
    'If the Nakba characterization is accepted as normatively dispositive, this reading''s high extraction and snare classification are well-grounded. If subsequent international recognition and prescriptive state practice are held to cure the founding harm (per international law''s general treatment of long-settled territorial facts), the extraction assessment would shift substantially downward, converging toward the partition_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_event_characterization, conceptual, 'Whether the founding event''s characterization is normatively dispositive for legitimacy.').

omega_variable(
    right_of_return_implementability,
    'Is the right of return, as this reading holds it exists, structurally implementable at this point (three-plus generations after 1948) without producing a comparably severe displacement of the current Jewish Israeli population, or has time itself created a second layer of settled-population facts this reading must also address?',
    'Demographic and policy modeling of return scenarios; comparative study of other post-conflict return and restitution regimes (e.g., post-apartheid land restitution, post-Yugoslav return processes) for implementability precedent.',
    'If implementable without comparable displacement, the reading''s remedy is coherent as stated. If not, the reading faces an internal tension between its founding-harm diagnosis and its proposed remedy that would need to be addressed either through a graduated remedy framework or through acknowledging a second, more recent settled-population problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(right_of_return_implementability, empirical, 'Whether the central remedy (right of return) is practically implementable given demographic change since 1948.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t1987, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1987, 0.3).
narrative_ontology:measurement_basis(terr_tr_t1987, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2010, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2010, 0.39).
narrative_ontology:measurement_basis(terr_tr_t2010, observed).
narrative_ontology:measurement(terr_tr_t2024, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(terr_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.7).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.78).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t1987, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement_basis(terr_be_t1987, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2010, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2010, 0.85).
narrative_ontology:measurement_basis(terr_be_t2010, observed).
narrative_ontology:measurement(terr_be_t2024, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2024, 0.87).
narrative_ontology:measurement_basis(terr_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.65).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t1987, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1987, 0.8).
narrative_ontology:measurement_basis(terr_su_t1987, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2000, 0.83).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2010, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement_basis(terr_su_t2010, observed).
narrative_ontology:measurement(terr_su_t2024, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2024, 0.88).
narrative_ontology:measurement_basis(terr_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, security_necessity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_legitimacy kernel (indigenous_continuity_reading, partition_reading, security_necessity_reading), each authored as a separate constraint story per the epsilon-invariance principle. The indigenous_continuity_reading authors the highest extraction and a snare classification because, by its own normative lights, the founding coordination story is cover for unremedied dispossession. The forecloses relation to partition_reading reflects that this reading's core premise (the 1948 partition instrument is void by dispossession) directly negates partition_reading's core premise (the partition instrument is the valid legal foundation) — no single legitimacy framework can hold both as simultaneously true. The influences relation to security_necessity_reading reflects that this reading's account of 1948 as ongoing dispossession creates downstream normative pressure on security-necessity arguments (framing post-1967 territorial control as compounding rather than defending against an original harm) without directly foreclosing the security_necessity_reading's distinct claim about 1967-era defensive necessity, which rests on different facts and a different time horizon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
