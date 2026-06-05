% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute Jurisdiction—Sovereigntist Reading (Conditional Consent Framework)
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute of the International Criminal Court (1998, entered force
 *   2002) creates a treaty framework for prosecuting genocide, crimes against
 *   humanity, and war crimes. The sovereigntist reading interprets the
 *   Statute as establishing a conditional jurisdiction framework where ICC
 *   authority strictly depends on state consent. Under this reading, the
 *   Statute's core legitimating principle is state sovereignty: the ICC can
 *   only prosecute nationals of states that have ratified the Statute or been
 *   referred by the UN Security Council. Non-consenting states (including
 *   permanent Security Council members and major powers like the United
 *   States, Russia, China, and India) retain absolute immunity for their
 *   nationals. National courts retain primary jurisdiction through
 *   complementarity—the ICC acts only when national systems are unwilling or
 *   unable. This reading emphasizes that the Statute is a contractual
 *   agreement among consenting parties, not a universal legal order. The
 *   extraction mechanism operates through the asymmetry between consenting
 *   and non-consenting states: victims in non-consenting state contexts
 *   receive no ICC recourse, while nationals of consenting states face
 *   potential prosecution. The theater ratio reflects that the consent
 *   requirement, while textually real, obscures the universal aspiration
 *   embedded in the Statute's preamble and Articles 12(3) and 13(b), which
 *   permit jurisdiction beyond strict consent. Over the 20-year measurement
 *   interval (2002–2022), extractiveness has risen as the gap between
 *   aspiration and structural reality has become more salient: the ICC's case
 *   load concentrates on African states (mostly consenting), while crimes by
 *   nationals of powerful non-consenting states remain untouched. Theater has
 *   also risen as the international community's rhetoric about universal
 *   justice has intensified while the structural consent requirement remains
 *   intact.
 *
 * KEY AGENTS:
 *   - Non-consenting great powers (US, Russia, China, India): Beneficiaries of immunity (institutional/arbitrage) — capture the benefit of exempting their nationals from ICC jurisdiction while participating in international norm-setting that condemns atrocities
 *   - Consenting states (primarily African, smaller developed democracies): Participants in coordination (institutional/constrained) — ratify the Statute and submit to complementarity mechanism; retain primary authority but accept ICC oversight
 *   - Victims of atrocities in non-consenting state contexts: Victims (powerless/trapped) — bear the cost of structural immunity; if perpetrator and victim both nationals of non-consenting state, ICC provides no recourse
 *   - National judiciaries in consenting states: Secondary institutional actor (institutional/constrained) — retain primary jurisdiction but face ICC supplementarity as external standard they did not design
 *   - International criminal law community: Analytical observer (analytical/analytical) — risks naturalizing consent as immutable while observing that other international regimes (ILOs, human rights courts) operate without absolute consent requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.38).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.52).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute Jurisdiction—Sovereigntist Reading (Conditional Consent Framework)").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, '6d72120d-e5e3-4189-b450-4d3355e3df17').
narrative_ontology:cs_kernel_codification('6d72120d-e5e3-4189-b450-4d3355e3df17', formalized).
narrative_ontology:cs_authority_grounding('6d72120d-e5e3-4189-b450-4d3355e3df17', extraction).
narrative_ontology:cs_interpretation_layer_present('6d72120d-e5e3-4189-b450-4d3355e3df17').
narrative_ontology:cs_reading_relation('6d72120d-e5e3-4189-b450-4d3355e3df17', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d72120d-e5e3-4189-b450-4d3355e3df17', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_axiom('6d72120d-e5e3-4189-b450-4d3355e3df17', foundational, state_consent_is_foundational_legitimacy).
narrative_ontology:cs_axiom_status(state_consent_is_foundational_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('6d72120d-e5e3-4189-b450-4d3355e3df17', state_consent_is_foundational_legitimacy, conventional).
narrative_ontology:cs_axiom('6d72120d-e5e3-4189-b450-4d3355e3df17', foundational, complementarity_as_deference_to_state_authority).
narrative_ontology:cs_axiom_status(complementarity_as_deference_to_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('6d72120d-e5e3-4189-b450-4d3355e3df17', complementarity_as_deference_to_state_authority, conventional).
narrative_ontology:cs_reference_frame('6d72120d-e5e3-4189-b450-4d3355e3df17', sovereign_equality_and_contractual_obligation).
narrative_ontology:cs_drift_state('6d72120d-e5e3-4189-b450-4d3355e3df17', contemporary_accountability_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6d72120d-e5e3-4189-b450-4d3355e3df17', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, consenting_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, national_judiciaries).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, non_consenting_state_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, universal_accountability_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CONSENTING STATE NATIONAL (SNARE) — Nationals of non-party states (e.g., citizens of the United States, Russia, China, India) face structural immunity from ICC prosecution regardless of conduct. The constraint operates as pure extraction: the non-consenting state captures the benefit of exempting its nationals, while the victim of the alleged crime bears the cost of judicial unavailability. Maximum experienced extraction with no exit option short of state ratification, which the national cannot control.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSENTING STATE (ROPE) — States party to the Rome Statute benefit from the ICC as a coordination mechanism for international criminal justice. The state retains primary jurisdiction through complementarity (ICC only acts when national courts are unwilling or unable); the state controls whether to refer situations, and can withdraw with one year's notice. The constraint functions as pure coordination from this perspective: the state's nationals face potential ICC jurisdiction, but the state chose this in exchange for participating in a system that also offers tools for prosecuting crimes by nationals of non-consenting adversaries.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: VICTIM IN NON-CONSENTING STATE (TANGLED ROPE) — A victim of genocide or crimes against humanity committed by nationals of a non-consenting state (perpetrator and victim both nationals of, e.g., the US or Russia) faces a mixed constraint. The ICC cannot prosecute because neither state consented. But complementarity creates a second avenue: if the victim's own state (non-consenting) is unwilling or unable to prosecute, the victim has no recourse at the international level. Coordination function exists (the Rome Statute creates accountability mechanisms for consenting state situations), but asymmetric extraction operates at this victim's cost: their crime receives no international justice pathway.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONAL JUDICIARY (TANGLED ROPE) — The complementarity mechanism creates a coordination function: national courts are incentivized to prosecute serious crimes domestically, avoiding the reputational cost of ICC referral. But complementarity also embeds asymmetric extraction: the national court's authority is *defined* by its subordination to the ICC's trigger threshold. A national court that proceeds slowly or half-heartedly against regime allies will be judged against ICC standards it never agreed to. The court is both empowered (retains primary authority) and constrained (subject to ICC supplementarity).
constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL COMMUNITY CONSENSUS (PITON) — The sovereigntist reading interprets the Rome Statute's consent requirement as a supreme principle—a procedural ritual that certifies state authority over the ICC. The mechanism is largely performative: the Statute's preamble and substantive articles express universal norms against genocide and crimes against humanity, yet the operative gate is state ratification. The international consensus persists in the theater of the Rome Statute despite the structural reality that most of the world's population (citizens of non-consenting great powers) remains outside the system. Institutional inertia maintains the consent ritual as the legitimating mechanism.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SOVEREIGNTY AS NATURAL LIMIT (MOUNTAIN) — From a civilizational perspective, state sovereignty is treated as an immutable natural limit on international authority. Under this reading, the Rome Statute cannot transcend consent because consent IS the legitimating principle of international law itself. No authority can bind a non-consenting state without violating the foundational law of nations. This perspective sees the constraint as a structural feature of the international legal order itself—unchangeable and universal.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rome_statute_jurisdiction__sovereigntist_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, TR),
    TR >= 0.70.

:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The sovereigntist reading embeds asymmetric extraction between consenting and non-consenting states, and between victims in consenting vs. non-consenting state contexts. However, the extraction is not maximal because consenting states genuinely benefit from the coordination function (the ICC enables prosecution of crimes by nationals of other consenting states, and UNSC referral creates a partial bypass mechanism for non-consenting state situations). The value reflects that the constraint is hybrid: part coordination mechanism, part immunity structure. Suppression (0.52): Moderate-high. Non-consenting state nationals face substantial barriers to accountability—no ICC path without UNSC referral, and national courts in non-consenting states often lack capacity or political will. But suppression is not total because consenting states can coordinate pressure on non-consenting states, and UNSC referral (rare but real) provides a bypass. Theater ratio (0.45): Moderate. The consent requirement is not purely performative—ratification by consenting states is structurally meaningful, and national court jurisdiction is real. But theater exists in the gap between the Statute's universal rhetoric (Preamble, universal norm language) and its operationally limited reach (consent gate). The Statute performs universal justice aspiration while delivering conditional accountability.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why a single treaty framework generates multiple readings. The consenting state sees coordination (Rope)—the ICC enables prosecution of crimes against their nationals and provides a forum for collective accountability. The non-consenting state national sees snare (Snare)—structural immunity from a system they never consented to, while the international community repeatedly condemns the crimes they're exempt from. The victim in a non-consenting state context sees tangled rope (Tangled Rope)—the Rome Statute creates accountability mechanisms, but the complementarity mechanism that should protect them depends on their own state's willingness to prosecute. The national judiciary sees tangled rope (Tangled Rope)—they retain authority but are judged against an external supplementarity standard. The international consensus sees the sovereignty ritual as supreme (Piton)—the consent requirement persists as the legitimating mechanism despite universal aspiration rhetoric. The analytical observer risks seeing sovereignty as a natural limit (Mountain), but structural analysis reveals it as a contingent institutional choice: the treaty was negotiated, and consent could have been structured differently (as the UNSC bypass partly demonstrates).
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereigntist reading positions consenting states as institutional beneficiaries with arbitrage options (they can withdraw with notice, Article 127) and non-consenting states as institutional beneficiaries with broader arbitrage (they remain outside the system entirely). Non-consenting state nationals are victims facing trapped exit (their state will not ratify, and they cannot force ratification). Victims in non-consenting state contexts are powerless and trapped—no international recourse available. National judiciaries in consenting states are institutional agents facing constrained exit: they retain primary authority but are subordinated to ICC supplementarity. The analytical observer derives d from the structure of immunity (high d toward non-consenting state nationals, low d toward consenting states), producing moderate overall χ because the constraint coordinates some activity (among consenting states) while extracting from others (non-consenting state nationals).
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereigntist reading resolves mandatrophy by distinguishing between the Statute's universal aspiration (Preamble, norms language) and its conditional operative framework (consent requirement). The mandatrophy—tension between universal accountability and state autonomy—is not resolved; it is formalized. The sovereigntist reading prioritizes autonomy over universalism, making consent the supreme principle. This choice is defensible (state sovereignty is a real principle of international law) but contestable (other international regimes operate without absolute consent). The reading's strength is that it acknowledges the fundamental tension and places sovereignty at the center. Its weakness is that it naturalizes sovereignty as immutable, obscuring that the Rome Statute could have been written differently. The hybrid_complementarity and universalist readings offer alternative hierarchies of the same tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_legitimacy_vs_justice_access,
    'Does the requirement for state consent to ICC jurisdiction create a natural limitation on international authority, or does it represent a contingent institutional choice that privileges state immunity over victim access to justice?',
    'Comparative analysis of treaty frameworks: examine other international mechanisms (ILOs, human rights courts, environmental tribunals) and whether they embed absolute consent requirements or permit authority without consent. Normative analysis: distinguish between consent as epistemically necessary (states must agree to be bound) versus consent as politically negotiated (states chose to retain immunity in the Rome Statute despite universal aspirations).',
    'If consent is natural/necessary: the constraint is a Mountain—immutable limit on international law. The sovereigntist reading is correct. If consent is contingent/chosen: the constraint is a Tangled Rope—states chose to embed consent in the text to preserve authority. The universalist reading is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_legitimacy_vs_justice_access, conceptual, 'Whether consent is an immutable natural limit or a contingent institutional choice').

omega_variable(
    complementarity_as_deference_or_subordination,
    'Does complementarity (ICC deference to national courts) represent genuine state authority and coordination, or does it represent subordination of national courts to an international standard they did not consent to?',
    'Longitudinal analysis of ICC complementarity decisions: examine cases where the ICC deemed national proceedings ''unwilling or unable,'' and assess whether those determinations respected the structural authority of the national court or imposed external standards. Survey national court judges on whether complementarity feels like coordination or subordination. Examine the institutional asymmetry: can national courts override ICC determinations, or is the ICC''s authority one-directional?',
    'If complementarity respects national authority: tangled rope classification correct (genuine coordination + some extraction). If complementarity subordinates national courts: snare classification for national judiciaries (pure extraction of judicial authority masked as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_as_deference_or_subordination, empirical, 'Whether complementarity functions as coordination between equals or subordination of national courts').

omega_variable(
    non_consent_immunity_scope,
    'Is immunity from ICC prosecution for nationals of non-consenting states limited to individuals (personal immunity), or does it extend to the state itself (state immunity from international claims)?',
    'Treaty interpretation: examine whether the Rome Statute''s consent requirement applies to individuals or states. Examine UN Security Council referrals (Article 13(b)): does UNSC referral create individual liability for non-consenting state nationals, or does it bypass state immunity? Precedent analysis: examine ICTY and ICTR, which operated without universal consent—did they establish state or individual accountability?',
    'If individual immunity only: non-consenting state nationals can be prosecuted via UNSC referral; constraint is less severe. If state immunity extends to nationals: non-consenting states can shield populations; constraint is more severe (closer to pure snare for victims).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_consent_immunity_scope, empirical, 'Scope of immunity—whether it extends to individuals or states').

omega_variable(
    sovereignty_as_reading_artifact,
    'Is the sovereigntist reading''s emphasis on consent a reading of the Rome Statute''s text, or a reading imposed by prior commitment to sovereignty as supreme value?',
    'Textual analysis: compare the Rome Statute''s Preamble (which emphasizes universal norms against atrocities) with its operative articles (which require consent). Examine the structure: if the Statute''s framers truly believed consent was supreme, why include universal jurisdiction provisions (Articles 12(3) and 13(b)) that partially bypass consent? Historical analysis: examine negotiation records—did states with the most power resist universal jurisdiction most vigorously?',
    'If consent is textually primary: sovereigntist reading is authentic. If universal norms are textually weighted equally: sovereigntist reading is a selective reading privileging sovereignty over universalism. The constraint''s ε would shift upward if the reading is recognized as contingent rather than necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_as_reading_artifact, conceptual, 'Whether the sovereigntist reading is textually grounded or imposed by prior commitments').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_sov_tr_t0, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(rome_sov_tr_t10, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 10, 0.42).
narrative_ontology:measurement(rome_sov_tr_t20, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(rome_sov_be_t0, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(rome_sov_be_t10, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(rome_sov_be_t20, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).

% DUAL FORMULATION NOTE:
% The rome_statute_jurisdiction kernel generates three distinct constraint stories, each modeling a different reading of the same treaty. The sovereigntist_reading models the constraint where consent is supreme and complementarity is deference to state authority. The hybrid_complementarity_reading models the constraint where complementarity balances universal and sovereign principles. The universalist_reading models the constraint where universal jurisdiction transcends consent. Each has its own ε (0.38, 0.52, 0.58) and its own type classification. These are not different observables of the same constraint; they are different readings of a contested kernel, and the disagreement is conceptual and normative, not empirical.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
