% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215: Baronial Privilege Reading (Feudal Contract, Limited Protection)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   Magna Carta (1215) is a feudal contract between King John and a coalition
 *   of barons, formalized as a response to the king's arbitrary extraction of
 *   feudal incidents (wardship, reliefs, marriage fees, scutage). The charter
 *   limits these prerogatives, protecting baronial property rights and
 *   establishing procedures for taxation. The constraint instantiated by the
 *   baronial_privilege_reading is the charter as feudal contract: 'free men'
 *   are landowning barons and church institutions; protection applies only to
 *   contracting parties; extraction is redirected from crown-to-baron onto
 *   baron-to-peasant. This reading is structurally distinct from the
 *   universal_rights_reading (which retrojects Enlightenment principles of
 *   human liberty onto 1215 language) and from the living_document_reading
 *   (which treats the charter as an evolving principle rather than a fixed
 *   feudal contract). The baronial_privilege_reading is the empirically
 *   grounded historical constraint — the actual 1215 document and its
 *   13th-century enforcement. The other readings emerge later through textual
 *   reinterpretation. This constraint demonstrates how a single text can
 *   ground structurally distinct constraints depending on which reading is
 *   active.
 *
 * KEY AGENTS:
 *   - Landowning Barons: Primary beneficiaries (powerful/arbitrage) — coordinate collective demand to constrain crown revenue extraction; extract surplus from peasant rents and labor to compensate for reduced crown levies
 *   - Church / Ecclesiastical Landholders: Secondary beneficiaries (institutional/arbitrage) — protected explicitly in Charter; maintain institutional prestige as oath-guarantor but enforcement capacity atrophies
 *   - King John and Crown Authority: Constrained party (institutional/constrained) — loses arbitrary revenue extraction mechanisms; gains stability and legitimacy; eventually learns to work within charter framework
 *   - Common Subjects / Non-Landowning Freemen: Secondary victims (moderate/constrained) — nominally included in 'free men' but receive no actual protection; bear increased extraction as crown redirects revenue-raising onto non-baronial populations
 *   - Peasant / Villain Populations: Primary victims (powerless/trapped) — explicitly excluded from charter protection; trapped in serfdom; bear intensified baronial extraction (increased rents and obligations) as barons compensate for crown revenue loss
 *   - Analytical Observer: Civilizational/universal (analytical/analytical) — risks reading the charter as natural law (mountain perspective) when it is contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.58).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.62).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215: Baronial Privilege Reading (Feudal Contract, Limited Protection)").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, 'ab8cb747-8a46-4ecd-b84a-bb291e36935a').
narrative_ontology:cs_kernel_codification('ab8cb747-8a46-4ecd-b84a-bb291e36935a', fixed_text).
narrative_ontology:cs_authority_grounding('ab8cb747-8a46-4ecd-b84a-bb291e36935a', extraction).
narrative_ontology:cs_interpretation_layer_present('ab8cb747-8a46-4ecd-b84a-bb291e36935a').
narrative_ontology:cs_reading_relation('ab8cb747-8a46-4ecd-b84a-bb291e36935a', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('ab8cb747-8a46-4ecd-b84a-bb291e36935a', magna_carta_living_document_reading, coexists_with).
narrative_ontology:cs_axiom('ab8cb747-8a46-4ecd-b84a-bb291e36935a', foundational, free_men_equals_landowning_barons).
narrative_ontology:cs_axiom_status(free_men_equals_landowning_barons, holdable).
narrative_ontology:cs_axiom_grounding('ab8cb747-8a46-4ecd-b84a-bb291e36935a', free_men_equals_landowning_barons, conventional).
narrative_ontology:cs_axiom('ab8cb747-8a46-4ecd-b84a-bb291e36935a', foundational, protection_limited_to_contracting_parties).
narrative_ontology:cs_axiom_status(protection_limited_to_contracting_parties, holdable).
narrative_ontology:cs_axiom_grounding('ab8cb747-8a46-4ecd-b84a-bb291e36935a', protection_limited_to_contracting_parties, deontological).
narrative_ontology:cs_reference_frame('ab8cb747-8a46-4ecd-b84a-bb291e36935a', feudal_contractual_authority).
narrative_ontology:cs_drift_state('ab8cb747-8a46-4ecd-b84a-bb291e36935a', late_thirteenth_century, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ab8cb747-8a46-4ecd-b84a-bb291e36935a', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, church_institutional_landholders).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, crown_revenue_authority).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, common_subjects).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, peasant_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LANDOWNING BARON (ROPE) — Experiences Magna Carta as pure coordination mechanism solving a collective action problem among barons. The constraint coordinates their joint demand (limit arbitrary taxation and wardship exploitation) and establishes mutual enforcement. Beneficiary with high exit options (can defect, renegotiate, appeal to Church) perceives low effective extraction. The charter is their victory — a mechanism to constrain the king's extractive prerogatives over feudal property.
constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMON SUBJECT / NON-LANDOWNING FREEMAN (TANGLED ROPE) — Falls nominally within 'free men' language but has no actual protection. Experiences both genuine coordination benefit (constraint on arbitrary royal power does reduce violence and chaos) AND asymmetric extraction (the crown's constrained revenue-raising mechanisms are redirected onto non-baronial populations; sheriffs extract more aggressively from common subjects to compensate for lost wardship income). Constrained exit — cannot organize to enforce the charter, cannot appeal to Church, cannot negotiate alternative terms.
constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PEASANT / VILLAIN POPULATION (SNARE) — Completely excluded from the charter's protection. Explicitly NOT 'free men.' Trapped by serfdom, bound to land, subject to manorial courts and baronial extraction. The charter's constraint on royal wardship and arbitrary taxation cascades into intensified baronial extraction from their labor (increased peasant obligations and rents to compensate barons for crown revenue losses). Zero exit options, zero coordination benefit. Pure extraction with suppression.
constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: CHURCH INSTITUTIONAL AUTHORITY (PITON) — The Church witnessed the 1215 baronial coup and benefited from explicit protections (Charter clause 1: 'Church shall be free'). But the Church's performative role in enforcing the charter (as spiritual guarantor of oaths, as mediator in disputes) has atrophied by the late 13th century. The Church retains institutional prestige and some claim to enforce baronial compliance but actual enforcement capacity has shifted to baronial councils and Parliament. The charter has become a ritual reaffirmation of mutual baronial obligation, not a living enforcement mechanism. Theater ratio rises as the Church's role becomes symbolic.
constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational distance, one might read Magna Carta 1215 as crystallizing an immutable principle: contract limits arbitrary power. But this reading naturalizes a contingent historical arrangement. The 'natural law' framing occludes the charter's actual function: formalization of baronial privilege as against royal extraction, with no universal principle of human rights. The natural law view is a false summit — it retrojects later (13th+ century) reinterpretations into the original 1215 text.
constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: ROYAL CROWN AUTHORITY (TANGLED ROPE) — The king is constrained by the charter but also uses it as a coordination mechanism: it stabilizes the feudal relationship, reduces ruinous baronial revolt costs, and creates a predictable framework for taxing and governing. The crown's extractiveness is reduced (no more arbitrary wardship, reliefs, marriage fees) but stabilized at a sustainable level. The crown has both loss (revenue reduction) and gain (stability and legitimacy). Constrained exit — the crown could repudiate the charter (John tried; it failed; the charter's baronial enforcer network persists). The crown eventually learns to work within the charter framework, making it generative of new administrative mechanisms rather than purely constraining.
constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(magna_carta_1215__baronial_privilege_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, TR),
    TR >= 0.70.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The charter constrains one extraction flow (crown-to-baron) while leaving another intact and redirecting a third (king-to-baron extraction is replaced with baron-to-peasant extraction). The net extractiveness reflects that: (a) the charter genuinely coordinates baronial interests, reducing internal extraction among barons; (b) the crown's extraction is reduced by charter terms; (c) peasant extraction is maintained at high levels and may increase as barons shift extraction burden downward to compensate for crown losses. Suppression (0.62): High. The charter is enforced through baronial councils, Church authority, and threat of renewed baronial rebellion. The suppression value reflects both the mechanism needed to enforce charter terms (against crown resistance) and the suppression underlying peasant subjection (manorial courts, serfdom, bound labor). Theater ratio (0.48): Moderate-low in period 0 (1215 immediately after sealing), rising to 0.48 by period 50 (late 13th century). The charter begins as a functional enforcement mechanism — real constraint on crown revenue extraction, real coordination device among barons. By late 13th century, the charter's ritual reaffirmation (reissues in 1216, 1217, 1225, etc.) has partly become performative — the baronial enforcement network remains but the document's role shifts from crisis instrument to constitutional reference point. Theater rises as the Church's enforcement role becomes symbolic and as crown compliance becomes normalized (less need for active enforcement, more reliance on institutional expectation).
 *
 * PERSPECTIVAL GAP:
 *   The critical gap is between the baron-beneficiary experience (coordination, Rope) and the peasant-victim experience (pure extraction, Snare). Both experiences are correct relative to their structural position. The charter genuinely coordinates baronial interests AND genuinely intensifies peasant extraction. The perspective that mistakes this for a universal rights document (analytical false summit) misses both facts — it reads the charter as establishing universal principles when it establishes only baronial protections, and it misses how those protections are funded through intensified peasant extraction. The perspectival gap is not disagreement about facts but disagreement about scope — does 'free men' include peasants or only barons? The 1215 answer is clear (only barons), making universal_rights_reading a later retroinjection rather than a 1215 fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from beneficiary/victim status and exit options. Barons as beneficiaries with arbitrage exit (can renegotiate, appeal to Church, defect) receive low d and low effective extraction chi. Peasants as victims with trapped exit (serfdom, no legal recourse, no organizational capacity) receive high d and high f(d), experiencing maximum chi. Common subjects as partial victims with constrained exit (can appeal to limited extent, can attempt relocation, but face barriers) receive moderate-high d. The crown as constrained party (could repudiate charter but enforcement network persists) has mid-range d reflecting mixed beneficiary-victim position. The directionality flow: extraction runs from crown toward barons (barons benefit), redirects from barons toward peasants (peasants bear cost), with common subjects caught in between. No directionality overrides needed — the structural relationships are unambiguous.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through explicit scope limitation: the charter protects barons, not all subjects. There is no claim that Magna Carta 1215 is a universal rights document — the baronial_privilege_reading is precise about limited application. The mandatrophy would arise if one conflated this reading with the universal_rights_reading, which claims the same document established universal protections. The two readings are in deliberate tension. The engine's false_summit detector will flag the analytical observer's mountain perspective as naturalization of contingent baronial arrangement. Resolution: the mountain perspective is analytically coherent (from civilizational distance, one might see the charter as crystallizing immutable principles) but empirically false — the charter is explicitly and intentionally limited to contracting parties, not universal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_rights_retrojection,
    'Is the 1215 Magna Carta''s ''free men'' language a genuinely universal rights principle emergent in 1215, or a retrojection of later (14th-16th century) reinterpretation back onto the original baronial contract?',
    'Textual and contextual analysis: compare 1215 charter language and contemporary chronicler interpretation with 14th-century legal commentaries (Bracton, Littleton) and 16th-century political theorists (Coke). Map the timeline of semantic drift from ''free men'' (landowning barons) to ''free men'' (universal principle).',
    'If genuine emergence: the baronial_privilege_reading is anachronistic; universal_rights_reading is present in 1215. If retrojection: baronial_privilege_reading is structurally accurate; universal_rights_reading is a later construction imposed onto the text. This determines whether the two readings truly coexist or one foreclosures the other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_rights_retrojection, empirical, 'Textual authenticity: is universal-rights language in 1215 or a later retroinjection?').

omega_variable(
    enforcement_mechanism_collapse,
    'When did the baronial enforcement network for Magna Carta lose functional capacity? Was it gradual (14th-15th century atrophy) or precipitous (dissolution by centralizing crown)?',
    'Historical record: frequency and effectiveness of baronial charter reissues; timeline of enforcement actions (charter-based litigation, baronial councils convening to enforce terms); correlation with shift from baronial to parliamentary enforcement mechanisms.',
    'If gradual collapse: the constraint transitions from Tangled Rope (enforcement active) to Piton (ritual without function) around 14th-15th century. If precipitous: clear turning point for reclassification. Either way, the temporal measurement trajectory shifts from flat to declining extractiveness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_mechanism_collapse, empirical, 'Timeline and mechanism of baronial enforcement network degradation').

omega_variable(
    peasant_protection_absence_structural,
    'Is the peasantry''s explicit exclusion from Magna Carta 1215 a structural necessity of the feudal system, or a contingent political choice by baronial drafters?',
    'Comparative feudalism: examine alternative feudal legal regimes (German, Italian, Iberian) where peasant protections or manorial constraints existed alongside nobility protections. If peasant protections appear in comparable feudal systems, exclusion was contingent; if absent, structural to feudalism.',
    'If structural: the peasantry''s snare classification is inherent to feudal arrangement; baron-peasant extraction is not a ''cost'' of the charter but a separate mechanism. If contingent: the charter''s drafters chose peasant exclusion, making it an asymmetric extraction feature of Magna Carta itself. This affects whether Magna Carta 1215 should include peasants in the victim set (it does here, presuming contingency).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_protection_absence_structural, empirical, 'Whether peasant exclusion was structural to feudalism or contingent to 1215 drafting').

omega_variable(
    reading_kernel_identity,
    'What is the kernel that both the baronial_privilege_reading and the universal_rights_reading read? Is it the 1215 document text, the principle of limited kingship, the concept of ''free'' persons, or something else?',
    'Meta-textual analysis: identify the invariant referent both readings appeal to and the points where they diverge in interpretation. If no single kernel is shared, the readings may not be siblings but wholly distinct constraints.',
    'If kernel is identified and shared: the reading_relations (forecloses, coexists_with, influences) are meaningful. If readings reference different kernels: they are separate constraint families, not siblings within one kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Identity of the shared kernel both sibling readings interpret').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_baron_tr_t0, magna_carta_1215__baronial_privilege_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(mc_baron_tr_t25, magna_carta_1215__baronial_privilege_reading, theater_ratio, 25, 0.38).
narrative_ontology:measurement(mc_baron_tr_t50, magna_carta_1215__baronial_privilege_reading, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(mc_baron_be_t0, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(mc_baron_be_t25, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(mc_baron_be_t50, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(mc_baron_su_t0, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(mc_baron_su_t25, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 25, 0.64).
narrative_ontology:measurement(mc_baron_su_t50, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_living_document_reading).

% DUAL FORMULATION NOTE:
% The Magna Carta kernel grounds three structurally distinct constraints: baronial_privilege_reading (ε=0.58, Tangled Rope, feudal contract, limited to contracting parties), universal_rights_reading (ε=0.32, Rope, universal human liberty principle, emerges through later reinterpretation), living_document_reading (ε=0.25, Scaffold, constitutional principle subject to sunset and renewal through reinterpretation cycles). The three readings diverge on kernel interpretation and victim/beneficiary scope. Each has its own ε, its own historical authority, and its own structural function. The baronial_privilege_reading is the empirically grounded 1215 constraint. The other readings emerge later and should be authored as separate constraint files with network links establishing the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
