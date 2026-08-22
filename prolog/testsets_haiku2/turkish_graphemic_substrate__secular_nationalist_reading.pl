% ============================================================================
% CONSTRAINT STORY: turkish_graphemic_substrate__secular_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_turkish_graphemic_substrate__secular_nationalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: turkish_graphemic_substrate__secular_nationalist_reading
 *   human_readable: Latin Script Mandate and National Linguistic Rupture (Secular Nationalist Reading)
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   Between 1928 and the mid-20th century, the Turkish Republic enforced a
 *   rapid transition from Arabic to Latin script, framed by the secular
 *   nationalist reading as a necessary break with Ottoman-Islamic heritage
 *   and alignment with European modernity. This constraint story instantiates
 *   the secular nationalist reading of Turkish linguistic identity: Turkish
 *   is understood as a distinct, ethnically-rooted national identity
 *   fundamentally severed from Ottoman continuity; Latin script is the
 *   legitimate graphemic substrate embodying that modernity and European
 *   alignment. This reading is contested by two sibling readings: the
 *   ottoman_continuity_reading (Turkish identity is continuous with
 *   Ottoman-Islamic civilization and Arabic script is legitimate) and the
 *   gradual_transition_reading (both scripts should coexist during a managed
 *   transition). The authored extractiveness (0.68), suppression (0.79), and
 *   theater_ratio (0.42) describe the constraint as it operated under this
 *   reading's frame—a tangled rope coordinating national linguistic
 *   unification while extracting from Ottoman-educated classes and religious
 *   communities. The claim-metric independence is deliberate: the secular
 *   nationalist reading CLAIMS this as legitimate coordination (rope)
 *   necessary for state formation; the authored metrics measure it as
 *   substantially extractive and actively suppressed operation (tangled rope
 *   / snare-adjacent). The engine computes the per-seat divergence from this
 *   structural data.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, 0.68).
domain_priors:suppression_score(turkish_graphemic_substrate__secular_nationalist_reading, 0.79).
domain_priors:theater_ratio(turkish_graphemic_substrate__secular_nationalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(turkish_graphemic_substrate__secular_nationalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(turkish_graphemic_substrate__secular_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(turkish_graphemic_substrate__secular_nationalist_reading, "Latin Script Mandate and National Linguistic Rupture (Secular Nationalist Reading)").
narrative_ontology:topic_domain(turkish_graphemic_substrate__secular_nationalist_reading, "political_linguistics/state_formation").

domain_priors:requires_active_enforcement(turkish_graphemic_substrate__secular_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(turkish_graphemic_substrate__secular_nationalist_reading, '3474b3a1-f883-4e80-a402-8294bdfb86d9').
narrative_ontology:cs_kernel_codification('3474b3a1-f883-4e80-a402-8294bdfb86d9', fixed_text).
narrative_ontology:cs_authority_grounding('3474b3a1-f883-4e80-a402-8294bdfb86d9', extraction).
narrative_ontology:cs_interpretation_layer_present('3474b3a1-f883-4e80-a402-8294bdfb86d9').
narrative_ontology:cs_reading_relation('3474b3a1-f883-4e80-a402-8294bdfb86d9', turkish_graphemic_substrate__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('3474b3a1-f883-4e80-a402-8294bdfb86d9', turkish_graphemic_substrate__gradual_transition_reading, coexists_with).
narrative_ontology:cs_axiom('3474b3a1-f883-4e80-a402-8294bdfb86d9', foundational, turkish_identity_distinct_from_ottoman_islamic).
narrative_ontology:cs_axiom_status(turkish_identity_distinct_from_ottoman_islamic, holdable).
narrative_ontology:cs_axiom_grounding('3474b3a1-f883-4e80-a402-8294bdfb86d9', turkish_identity_distinct_from_ottoman_islamic, deontological).
narrative_ontology:cs_axiom('3474b3a1-f883-4e80-a402-8294bdfb86d9', foundational, latin_script_embodies_european_modernity).
narrative_ontology:cs_axiom_status(latin_script_embodies_european_modernity, holdable).
narrative_ontology:cs_axiom_grounding('3474b3a1-f883-4e80-a402-8294bdfb86d9', latin_script_embodies_european_modernity, empirically_contingent).
narrative_ontology:cs_reference_frame('3474b3a1-f883-4e80-a402-8294bdfb86d9', ottoman_islamic_linguistic_continuity).
narrative_ontology:cs_drift_state('3474b3a1-f883-4e80-a402-8294bdfb86d9', contemporary_post_mandate_period, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3474b3a1-f883-4e80-a402-8294bdfb86d9', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(turkish_graphemic_substrate__secular_nationalist_reading, turkish_graphemic_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_state_apparatus).
narrative_ontology:constraint_beneficiary(turkish_graphemic_substrate__secular_nationalist_reading, european_aligned_educated_elite).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_educated_class).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_clergy).
narrative_ontology:constraint_victim(turkish_graphemic_substrate__secular_nationalist_reading, illiterate_rural_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The early Turkish Republic's administrative and educational state structure mandates Latin script adoption, enforces it through schooling curricula and official communications, and suppresses Arabic script in public and legal contexts. The state frames this as scientific modernization and national unity; it directly controls educational policy, legal codification, and media licensing. This seat administers the constraint and benefits from the linguistic homogenization and severing of Ottoman continuity.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Urban, secular, Western-educated professionals and intellectuals who advocate for Latin script adoption as a marker of European identity and modernization. They gain cultural prestige, career advancement in the state bureaucracy, and enhanced prestige in international contexts by embodying and promoting the transition. They face no suppression; exit is available to those who wish to engage with Ottoman or Islamic scholarship, but the constraint provides status, professional opportunity, and alignment with the ruling ideology.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, european_aligned_educated_elite, beneficiary,
    powerful, generational, arbitrage, national).

% Ottoman-trained scholars, judges, scribes, and bureaucrats whose entire professional identity and knowledge base rested on Arabic script mastery and Ottoman-Islamic learning traditions. The mandate renders their literacy and expertise functionally obsolete within the state apparatus. Their options are retraining (which many cannot afford or access), leaving the profession, or persisting in marginalized private practice. Many carry identity fusion: their self-concept as learned people is bound to the Ottoman scholarly tradition.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_educated_class, payer,
    moderate, biographical, identity_locked, national).

% Islamic teachers, Quranic scholars, and religious authority figures whose teachings and texts are written in Arabic script. The script mandate undermines their ability to transmit knowledge to new generations trained in Latin script, weakening their institutional authority and capacity to interpret Islamic law and tradition. They are systematically excluded from state educational policy-setting and have constrained voice in decisions about script transition.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_clergy, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(turkish_graphemic_substrate__secular_nationalist_reading, religious_scholars_and_clergy, excluded).

% Rural and semi-literate populations who encounter the state's script mandate through administration, signage, and schooling they may not access. If previously exposed to Ottoman Arabic script through limited education or religious instruction, they face cognitive switching costs and exclusion from written state communication. If newly literate under the Latin script regime, they are cut off from the small corpus of Ottoman-era written materials they might encounter.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, illiterate_rural_populations, payer,
    powerless, biographical, trapped, national).

% A non-agent entity: the accumulated knowledge corpus, interpretive traditions, and intellectual lineages developed over centuries of Ottoman-Islamic civilization. The script mandate structurally prevents intergenerational transmission of this heritage to populations trained only in Latin script, effectively creating a civilizational rupture.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_islamic_heritage_transmission, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_islamic_heritage_transmission).

% International observers, including other nation-states, linguistic scholars, and cultural institutions, who document the script transition as either a model of successful modernization or as a case of imposed cultural erasure, depending on their ideological frame. They record the constraint's operation and attest to the degree of coercion involved.
narrative_ontology:constraint_stakeholder(turkish_graphemic_substrate__secular_nationalist_reading, international_observers_and_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(turkish_graphemic_substrate__secular_nationalist_reading, secular_nationalist_state_apparatus).
narrative_ontology:fixing_cost_class(turkish_graphemic_substrate__secular_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified graphemic and orthographic substrate for the entire nation-state territory, enabling standardized education, legal documentation, and administrative communication across diverse populations without the fragmentation that marked the Ottoman multilingual-script context.
% TRANSFER_FUNCTION: Moves the capacity for literacy, cultural authority, and connection to Ottoman-Islamic knowledge traditions from the Ottoman-educated class and religious scholars to the state-controlled education apparatus and European-aligned elite. The transfer is enforced through control of schooling, legal documentation, and state employment.
% ABSENT_VOICES: Ottoman scholars dispersed into retirement or private practice; Quranic and Islamic teaching communities operating at the margins; rural populations without access to state schooling; intellectuals in the diaspora and in other Islamic regions who would contest the reading of Turkish linguistic identity as fundamentally severed from Ottoman civilization.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate disappeared overnight, Ottoman-trained scholars and religious institutions would resume transmission of Arabic-script learning; state bureaucracy would fragment into dual-script documentation; educational curricula would reincorporate Ottoman texts; and the primary mechanism binding Turkish national identity to European modernity would be severed, forcing a renegotiation of the nation's self-conception.
% FOUNDING_PROBLEM: Ottoman script diversity and multi-lingual administration created barriers to uniform national education and state capacity; European powers conducted diplomacy and trade in Latin script; the newly formed Turkish nation-state perceived orthographic unification and script alignment with Europe as necessary for modernization and international standing.
% FOUNDING_PROBLEM_CORROBORATION: The secular nationalist state apparatus and international observers aligned with European modernization attest the founding problem as live and the script mandate as its solution. Ottoman-heritage communities and some international scholars specializing in Islamic and Ottoman history attest the founding problem was overstated (Ottoman script diversity was manageable) and that the mandate persists as cultural erasure rather than as a response to genuine coordination failure. Linguistic analysis independent of the state context shows Turkish phonetics are representable in both scripts with comparable efficiency, undermining claims of technical superiority.
narrative_ontology:disappearance_verdict(turkish_graphemic_substrate__secular_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(turkish_graphemic_substrate__secular_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(turkish_graphemic_substrate__secular_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(turkish_graphemic_substrate__secular_nationalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(turkish_graphemic_substrate__secular_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(turkish_graphemic_substrate__secular_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 because the constraint transfers literacy authority, cultural prestige, and intergenerational transmission power from Ottoman-educated classes to the state apparatus and European-aligned elite, without proportional compensation or integration. The constraint is not self-evidently superior to Ottoman script (both are phonetically adequate for Turkish), so persistence depends on state enforcement of schooling, legal documentation, and public signage. Suppression starts high (0.62) and rises to 0.79 as the first-generation Ottoman-trained cohort ages out and schooling becomes universally administered in Latin script—the suppression is both structural (legal barriers to Arabic script use in official contexts, curriculum exclusion) and increasingly internalized (new generations have no literacy in Arabic script and see Ottoman heritage as foreign). Theater ratio rises from 0.25 to 0.42 as the actual coordination function (script standardization) achieves saturation and enforcement increasingly takes the form of historical narrativization, museum curation, and nationalist commemoration of the script break itself, rather than substantive barrier maintenance. The temporal series models extraction accumulation: initial phase (t=0-10) is active enforcement during the transition; consolidation phase (t=10-60) is institutionalization through schooling and legal practice; late phase (t=60-100) is performative maintenance as the constraint is now naturalized and Ottoman heritage is increasingly rendered as 'interesting history' rather than a live alternative. The shared time grid ensures all metrics are authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (state apparatus) experiences the constraint as coordination: a genuine problem (script diversity, Ottoman-Islamic association impeding European alignment) solved by rational policy choice (Latin script adoption). Exit options for the state are analytical—it can deliberate and change course, but there is no external pressure forcing exit. From the Ottoman-educated and religious scholar seats, the same structure is experienced as enforced extraction: an alternative (Arabic script continuity, gradual transition, dual-script literacy) is available but actively suppressed. Their exit is constrained (leaving means abandoning their professional identity) or identity-locked (they cannot conceive of themselves as Turkish without Ottoman learning traditions). This perspectival gap is the structural basis for the tangled_rope classification: genuine coordination (linguistic unification for a modern nation-state) paired with asymmetric extraction (cultural erasure of Ottoman heritage).
 *
 * DIRECTIONALITY LOGIC:
 *   See above—combined under a single commentary section.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The founding problem (Ottoman script diversity impeded state capacity and European alignment) is CONTESTED. The state and European-aligned elites attest it as live and solved by the script mandate. Ottoman-heritage scholars and some international observers attest the problem was overstated (Ottoman script diversity was manageable) and the mandate persists as cultural erasure, not as a response to genuine coordination failure. The measurement series shows extractiveness plateaus after t=60 (institutional maturity) and theater ratio stabilizes at 0.42 (performative enforcement), consistent with the constraint shifting from active policy implementation to institutionalized inertia. The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges flags potential zombie-function dynamics: if the founding problem is no longer live (Ottoman script is no longer used, modern education is universal in Latin script, European alignment is secured), but the constraint persists through educational inertia and nationalist commemoration, the constraint has become a Piton—a former Rope whose primary function has atrophied but which persists due to institutional theater. This reading interprets the constraint as still actively extractive (suppression of Ottoman heritage in education and public discourse remains policy); the gradual_transition_reading and ottoman_continuity_reading would interpret the same temporal data as evidence of mandatrophy and call for remediation (reintroduction of Arabic script option, bilingual education, restoration of Ottoman heritage transmission).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression structural (external barriers: legal prohibition, curriculum exclusion, administrative enforcement) or internalized (cognitive exclusion: Ottoman heritage perceived as ''foreign,'' Arabic script seen as ''backward'')?',
    'Quasi-natural experiment: jurisdiction that removes structural barriers (legally permits Arabic script in education and administration) while maintaining Latin script as default. If suppression persists (Ottoman heritage remains culturally marginal, Arabic script uptake is minimal), the suppression is substantially internalized. If suppression declines and Arabic script literacy and Ottoman heritage study expand, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—removing barriers alone will not restore Ottoman heritage transmission because populations lack the cognitive and cultural framework to engage with it. If structural, focused policy change (curriculum revision, administrative permission) could reverse the constraint. The internalization dimension also affects identity_lock dynamics: if suppression is primarily internalized, trapped Ottoman scholars bear not only economic loss but also psychological/cultural exile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in the script mandate.').

omega_variable(
    phonetic_equivalence_disputed,
    'Is Latin script technically superior to Arabic script for representing Turkish phonetics, or are both scripts phonetically adequate with different trade-offs?',
    'Comparative linguistic analysis of phonetic representation efficiency and fidelity in both scripts, independent of nationalist ideology. Historical phonetic analysis of Ottoman Turkish and modern Turkish to assess whether the script change was motivated by linguistic necessity or by ideological preference for European alignment.',
    'If Latin script is technically superior, the constraint''s framing as ''scientific modernization'' is vindicated and extractiveness is lower (the constraint solves a real technical problem). If both are phonetically adequate, the technical justification is revealed as cover story for cultural erasure, and extractiveness is higher (the constraint is motivated by ideology, not necessity). This affects mandatrophy analysis: if the founding problem was technical inadequacy of Arabic script, it was solved; if it was cultural/political alignment, it persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(phonetic_equivalence_disputed, empirical, 'Technical adequacy of Arabic script vs. Latin script for Turkish phonetics.').

omega_variable(
    kernel_reading_contest,
    'Is Turkish linguistic identity structurally distinct from Ottoman-Islamic heritage (secular nationalist reading), continuous with Ottoman-Islamic civilization (ottoman continuity reading), or intermediate with managed transition possibilities (gradual transition reading)?',
    'Genealogical and etymological analysis of Turkish language and lexicon; historical study of Ottoman intellectual traditions and their continuity or discontinuity with post-Republican Turkish thought; social research on contemporary Turkish populations'' self-identification and relationship to Ottoman heritage; comparative analysis of how Turkish in diaspora communities (less subject to the nationalist script mandate) develops and relates to Ottoman vocabulary and heritage.',
    'The secular nationalist reading is core to this constraint story. If Turkish linguistic identity is empirically more continuous with Ottoman heritage than the nationalist reading claims, this reading forecloses an alternative that is empirically grounded, weakening its legitimacy. If Turkish identity is genuinely distinct, the reading''s framing is supported. If intermediate (both continuity and rupture are empirically present), the gradual_transition_reading gains credibility as a policy alternative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The fundamental contested question: Is Turkish identity distinct from or continuous with Ottoman-Islamic heritage? This is the core kernel question; reading-level answer shapes the entire constraint''s legitimacy.').

omega_variable(
    european_modernity_contingent_alignment,
    'Is alignment with European modernity contingent on Latin script adoption and explicit rupture with Ottoman heritage, or can ''modernity'' be achieved through other pathways (e.g., institutional reform, technological adoption, international engagement while maintaining Ottoman heritage literacy)?',
    'Comparative historical analysis of modernization pathways in other post-Ottoman and post-colonial nation-states; study of Turkey''s actual modernization trajectory (institutional, military, technological) and whether script alone was a causal factor or whether institutional change was sufficient; analysis of other European nation-states'' own linguistic and script histories and how they relate to ''modernity'' (many European languages had recent script changes, dialect standardization, etc.).',
    'If European modernity requires Latin script and Ottoman rupture, the constraint is justified as necessary for state formation. If modernity is achievable through other pathways, the constraint becomes contingent policy choice rather than structural necessity, raising the extraction reading and supporting mandatrophy analysis. This omega captures the ideological contingency: the secular nationalist reading links script to modernity; alternative readings would decouple them.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(european_modernity_contingent_alignment, preference, 'Is Latin script adoption and Ottoman heritage rupture necessary for modernization, or contingent political choice?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(turkish_graphemic_substrate__secular_nationalist_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(turk_tr_t0, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(turk_tr_t10, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(turk_tr_t20, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(turk_tr_t40, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement(turk_tr_t60, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(turk_tr_t100, turkish_graphemic_substrate__secular_nationalist_reading, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(turk_be_t0, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(turk_be_t10, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(turk_be_t20, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(turk_be_t40, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(turk_be_t60, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(turk_be_t100, turkish_graphemic_substrate__secular_nationalist_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(turk_su_t0, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(turk_su_t10, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(turk_su_t20, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement(turk_su_t40, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 40, 0.77).
narrative_ontology:measurement(turk_su_t60, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(turk_su_t100, turkish_graphemic_substrate__secular_nationalist_reading, suppression_requirement, 100, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(turkish_graphemic_substrate__secular_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(turkish_graphemic_substrate__secular_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, turkish_language_standardization__vocabulary_arabization).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, ottoman_bureaucratic_memory__archival_access).
narrative_ontology:affects_constraint(turkish_graphemic_substrate__secular_nationalist_reading, religious_education__quranic_transmission).

% DUAL FORMULATION NOTE:
% The turkish_graphemic_substrate kernel decomposes into three constraint stories corresponding to three structural readings: secular_nationalist_reading (this story—script rupture as modernization), ottoman_continuity_reading (script continuity as heritage preservation), and gradual_transition_reading (managed coexistence as compromise). These are not the same constraint viewed from different seats; they are structurally distinct constraints instantiated by different readings of the same contested kernel. Their ε values differ substantially because the referent (what counts as 'Turkish linguistic identity' and its proper graphemic substrate) differs across readings. This story and its siblings are linked via network.affects_constraints because policy in one reading (e.g., aggressive Latin script mandate) creates structural pressure on alternatives (gradual transition becomes harder, Ottoman continuity becomes more marginal).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(turkish_graphemic_substrate__secular_nationalist_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
