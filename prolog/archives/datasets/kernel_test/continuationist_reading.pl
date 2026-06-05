% ============================================================================
% CONSTRAINT STORY: continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuationist_reading, []).

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
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: continuationist_reading
 *   human_readable: Continuationist Reading: Polygamy Theologically Valid, Federal Law as External Duress
 *   domain: religious_authority/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The continuationist reading interprets the 1890 Manifesto as a prudential
 *   suspension of polygamous practice under federal duress, not a doctrinal
 *   rescission of the divine command for celestial marriage recorded in
 *   Doctrine and Covenants 132. Under this reading, polygamy remains
 *   theologically valid and binding, federal law is an external constraint
 *   that does not alter doctrine, and fundamentalist splinter groups that
 *   continue the practice maintain theological legitimacy within the
 *   framework of the original revelation. This reading creates a structural
 *   constraint by sustaining a closed interpretive community that defines
 *   itself against both mainstream Mormonism (which accepts the Manifesto as
 *   rescission) and federal law (which treats polygamy as criminal). The
 *   constraint operates by maintaining a distinction between doctrinal truth
 *   (polygamy is mandated) and prudential practice (polygamy is suppressed).
 *   The reading's force depends on preventing this distinction from
 *   collapsing — if federal law is framed as legitimate authority that can
 *   override doctrine, or if the Manifesto is read as rescission rather than
 *   suspension, the entire framework collapses. This makes the constraint
 *   highly dependent on suppression (federal pressure that validates the
 *   'duress' frame) and on enforcement of the interpretive boundary
 *   (community sanctions against 'compromised' interpretations). The reading
 *   exhibits extraction because the distinction between doctrine and practice
 *   creates asymmetric costs for different agents: leadership and male
 *   practitioners benefit from the theological legitimacy and authority
 *   structure; women and children bear costs through legal vulnerability,
 *   restricted exit options, and identity fusion with a framework that denies
 *   their material reality the status of doctrinal change.
 *
 * KEY AGENTS:
 *   - Continuationist Leadership: Primary beneficiary (institutional/arbitrage) — maintains authority claim and organizational distinctiveness through the reading; can exit or modify interpretation
 *   - Male Polygamy Practitioners: Secondary beneficiary (moderate/constrained) — theology legitimates their family structure and patriarchal authority; constrained exit due to community membership and identity fusion
 *   - Cohabiting Spouses: Primary victim (powerless/identity_locked) — legal status ambiguous, exit involves abandoning religious identity and kinship bonds
 *   - Children in Polygamous Households: Primary victim (powerless/trapped) — dependent on household economics, legal status uncertain, educational access limited, material barriers to exit combine with identity formation
 *   - Secondary Wives: Mixed (moderate/constrained) — coordination function provides theology of legitimacy; extraction through asymmetric property, inheritance, reproductive control
 *   - Fundamentalist Splinter Organizations: Organized actors (organized/constrained) — reading coordinates institutional identity and survival; constrained exit due to theological and organizational dependencies
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the reading's distinction between doctrine and practice as an immutable doctrinal feature rather than a contingent interpretive choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuationist_reading, 0.58).
domain_priors:suppression_score(continuationist_reading, 0.68).
domain_priors:theater_ratio(continuationist_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(continuationist_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(continuationist_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuationist_reading, tangled_rope).
narrative_ontology:human_readable(continuationist_reading, "Continuationist Reading: Polygamy Theologically Valid, Federal Law as External Duress").
narrative_ontology:topic_domain(continuationist_reading, "religious_authority/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuationist_reading, '1f12194c-911f-4f78-9a4f-9c27c32c3e31').
narrative_ontology:cs_created_at('1f12194c-911f-4f78-9a4f-9c27c32c3e31', '').
narrative_ontology:cs_kernel_codification('1f12194c-911f-4f78-9a4f-9c27c32c3e31', fixed_text).
narrative_ontology:cs_authority_grounding('1f12194c-911f-4f78-9a4f-9c27c32c3e31', lineage).
narrative_ontology:cs_interpretation_layer_present('1f12194c-911f-4f78-9a4f-9c27c32c3e31').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuationist_reading, fundamentalist_leadership).
narrative_ontology:constraint_beneficiary(continuationist_reading, polygamy_practitioners).
narrative_ontology:constraint_victim(continuationist_reading, cohabiting_spouses).
narrative_ontology:constraint_victim(continuationist_reading, children_legal_status).
narrative_ontology:constraint_victim(continuationist_reading, women_exit_options).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COHABITING SPOUSE (SNARE) — Structurally mobile (could divorce, relocate, contact authorities) but identity-locked through religious frame and kinship bonds. The reading that 'the Manifesto is duress, not doctrine' means their polygamous condition is theologically mandated and federal law is external persecution, not legitimate authority. Maximum suppression: emotional/identity fusion with the constraint. No alternative framing is internally available. Exit requires abandoning not just the marriage but the faith identity itself.
constraint_indexing:constraint_classification(continuationist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: CHILDREN IN POLYGAMOUS HOUSEHOLDS (SNARE) — Materially trapped: dependent on household economics, legal status uncertain (legitimacy doctrine creates inheritance and guardianship ambiguities), educational access constrained by isolation. The continuationist reading denies them the protection of the Manifesto's rescission — they are born into a theologically mandated condition with no exit until adulthood, and substantial barriers even then. Suppression is structural (dependency) and internalized (identity formation in isolated communities).
constraint_indexing:constraint_classification(continuationist_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: SECONDARY WIVES (TANGLED ROPE) — Experience mixed coordination and extraction. The reading provides a theology of legitimacy for their status (not concubinage, but covenant marriage) — genuine coordination function. Simultaneously, the constraint extracts labor, sexual access, and fertility under asymmetric property and inheritance rules. Exit is constrained by economic dependency, community sanctions, and identity fusion. The constraint both structures their community role and extracts from it.
constraint_indexing:constraint_classification(continuationist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FUNDAMENTALIST LEADERSHIP (ROPE) — The continuationist reading benefits institutional leadership by legitimating their authority claim and distinguishing them from mainstream Mormonism. They experience the constraint as coordination: it solves the problem of maintaining doctrinal continuity and mobilizing group identity against external pressure. Federal law is framed as external duress that justifies organizational separation. Leadership has exit option (arbitrage) — they can alter interpretation, accommodate to mainstream, or exit to diaspora. Net beneficiary.
constraint_indexing:constraint_classification(continuationist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: FUNDAMENTALIST SPLINTER ORGANIZATIONS (TANGLED ROPE) — Organized agents with constrained exit. The continuationist reading coordinates their internal identity and institutional survival (genuine coordination function: it explains why they maintain practices federal law prohibits). Simultaneously, the reading extracts from their members through enforcement of strict patriarchal authority, surveillance of dissent, and reproductive control. Organizations have some exit agency (schism, reformation) but face reputational and theological barriers. Mixed coordination-extraction.
constraint_indexing:constraint_classification(continuationist_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / DOCTRINAL IMMUTABILITY (MOUNTAIN) — From a civilizational/universal perspective on scriptural authority, the continuationist reading claims that certain doctrinal commitments (the Doctrine and Covenants Chapter 132 revelation on celestial marriage) are immutable across time and political pressure. The reading frames federal law as external constraint that cannot revise doctrine, only suppress practice. From this view, the structure is a natural law of religious authority: true doctrine is unchanging; temporal power cannot rescind revelation. However, the structural data reveals a false summit — identifiable beneficiaries (leadership, practitioners) exist, and the reading's force depends on enforcement (suppression). The 'immutable doctrine' framing naturalizes what is actually a contingent institutional choice about interpretation.
constraint_indexing:constraint_classification(continuationist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuationist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuationist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuationist_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The reading creates tangible asymmetries. Leadership benefits from the legitimacy claim and organizational control; practitioners benefit from theology that validates their family structure; women and children bear costs through legal vulnerability and restricted exit. The extractiveness is not as severe as pure extraction (ε > 0.70) because the reading does solve a genuine coordination problem for the community — it explains how to maintain doctrinal fidelity while complying with federal law (or technically, not complying while maintaining a theological justification). The 'solution' is extractive but it is a solution. Suppression (0.68): Moderately high. Federal law creates material barriers (criminal penalties, custody loss, property seizure). Community enforcement creates additional barriers (shunning, access restriction, reputation damage). Identity fusion means that even individuals not directly constrained by law experience suppression because the interpretive frame makes exit psychologically costly. The suppression has both structural (legal/economic) and internalized (identity/cognitive) components. Theater ratio (0.65): Moderate-high. The reading requires substantial performative work: maintenance of the distinction between doctrine (polygamy is mandated) and practice (polygamy is suspended), rhetorical framing of the Manifesto as duress not rescission, institutional separation from mainstream Mormonism to prevent the reading from being challenged by mainstream authority, and community surveillance to prevent members from adopting the substitutionist reading. The theater increases over time (from 0.55 to 0.68 across the interval) because the reading's stability increasingly depends on performative boundary maintenance rather than on theological argument — the cost of defending the reading against internal and external challenge has grown.
 *
 * PERSPECTIVAL GAP:
 *   The continuationist reading produces a wide perspectival gap because different agents have radically different relationships to the core claim (polygamy remains doctrinally valid). For leadership and practitioners, the reading is experienced as coordination that solves a theological crisis (how to maintain fidelity to revelation while complying with law, or at least having a theological justification for non-compliance). For women and children, the reading is experienced as Snare — it denies them the protection that the Manifesto could provide (legal legitimacy, rescission of the divine command) and instead locks them into a condition that is theologically mandated and therefore inescapable. For splinter organizations, the reading is Tangled Rope — it coordinates their identity but constrains their options. For the analytical observer, the reading risks appearing as a Mountain (immutable doctrinal principle) when it is actually a contingent institutional choice with identifiable beneficiaries. The gap reveals that the reading's force depends entirely on who you are within the structure — its coherence is not universal but perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position within the continuationist reading. Leadership and male practitioners benefit from the authority and legitimacy the reading provides (low d → negative effective extraction from their perspective); they have exit options (arbitrage — they can change interpretation, accommodate to law, or exit to diaspora). Women and children are targets of the reading's extractive mechanisms (high d → high effective extraction); they lack exit options (trapped or identity_locked — material barriers combine with identity fusion with the faith). Secondary wives occupy a middle position: the reading provides coordination function (legitimacy for their status) but extracts through asymmetric rules (moderate d → moderate χ). Fundamentalist splinter organizations have constrained exit (organizational/theological dependency) but some agency; they experience the reading as coordination (it sustains their institutional identity) but it also constrains them (high suppression of alternative interpretations). The analytical observer risks a mountain classification by treating the reading's distinction between doctrine and practice as an immutable principle of religious authority, when the structural data reveals beneficiaries (leadership, practitioners) who maintain the distinction through active enforcement (suppression, boundary policing). The false summit detector should flag this perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is ONE reading of a contested kernel (divine_marriage_command), not a unified claim. The continuationist reading (this file) is not the only structurally defensible reading of the same divine marriage commandment. The substitutionist reading (the Manifesto doctrinal rescission) and the coercion_visibility_reading (focus on federal suppression mechanism) would instantiate different constraints with different ε values and different beneficiary/victim structures. The continuationist reading solves the mandatrophy by acknowledging it: the constraint's classification depends on which reading of the kernel you adopt. This is not a defect in the framework; it is the framework correctly capturing that commitment-system constraints can have multiple coherent readings with different structural implications. The constraint's force is the force of the reading's institutional adoption, not the force of a universal principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_interpretation_ambiguity,
    'Is the 1890 Manifesto a doctrinal rescission of polygamy (substitutionist reading) or a prudential suspension under federal duress that leaves doctrine intact (continuationist reading)?',
    'Textual analysis of the Manifesto''s language (what is actually claimed), comparison with official church doctrine statements before and after, analysis of leadership statements about the Manifesto''s status, historical evidence of intent (was rescission the goal or survival strategy?), examination of whether the same interpretive frame allows for restoration if duress ended',
    'If rescission: women''s legal status is clarified, children''s legitimacy is restored, the constraint''s extractiveness drops significantly (ε → 0.15, type shifts to Piton). If suspension: current institutional structure persists, fundamentalist splinters maintain theological legitimacy for continued practice, the constraint remains Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manifesto_interpretation_ambiguity, conceptual, 'Whether the Manifesto rescinded polygamy doctrine or suspended practice under duress').

omega_variable(
    reading_dependency_on_suppression,
    'Would the continuationist reading maintain institutional force if federal suppression were removed (no legal penalty for polygamy)?',
    'Counterfactual analysis: if U.S. law permitted plural marriage, would mainstream Mormon institutional leadership continue reading the Manifesto as rescission, or would pressure emerge within fundamentalist wings to formally restore the doctrine? Historical pattern: did the reading''s stability depend on federal enforcement or on internal doctrinal commitments?',
    'If reading depends on suppression: it is a Snare sustained by external pressure, not an autonomous theological position (ε → 0.70, type → Snare). If reading is stable independent of suppression: it reflects genuine institutional choice, not duress (ε → 0.42, type remains Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_dependency_on_suppression, conceptual, 'Whether the continuationist reading''s stability depends on federal suppression').

omega_variable(
    identity_locked_binding_mechanism,
    'For women and children in continuationist communities, is the binding mechanism material (economic dependency, isolation, legal status uncertainty) or cognitive (identity fusion with doctrinal frame)?',
    'Post-exit trajectory analysis: do women who leave continuationist communities retain belief in polygamy''s doctrinal validity? Do they report that the constraint was experienced as divinely mandated or as imposed by community authority? Comparison of exit barriers: which are removed by exit (economic, legal, social) and which persist (internalized identity, belief in doctrine)?',
    'If primarily material: suppression is structural; exit becomes feasible with resources and legal protection (ε → 0.48). If primarily cognitive/identity-locked: exit requires identity reformation; internalized suppression persists after escape from community (ε → 0.65, suppression remains high even in new context).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_binding_mechanism, empirical, 'Whether suppression is material or identity-locked for women and children').

omega_variable(
    kernel_reading_instantiation,
    'This constraint instantiates the continuationist reading of the divine_marriage_command kernel. What specific observable signals distinguish this reading from the substitutionist_reading and coercion_visibility_reading sibling readings?',
    'Linguistic and behavioral signals: (1) Continuationist: polygamy remains doctrinally valid; Manifesto is suspension under duress; fundamentalist splinters are orthodox. (2) Substitutionist: Manifesto rescinded the doctrine; polygamy is now doctrinal error; fundamentalist splinters are apostate. (3) Coercion_visibility: focus on federal coercion mechanism (suppression observable) rather than doctrinal status; Manifesto''s legitimacy depends on visibility of duress. Each reading has distinct beneficiary/victim structure and ε value.',
    'Readings share kernel (divine_marriage_command) but produce different constraints: continuationist (this file, ε ≈ 0.58); substitutionist (ε ≈ 0.15, type Piton); coercion_visibility (ε ≈ 0.62, type Snare). The omega documents that this is one reading of a contested kernel, not a unified constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Kernel reading instantiation: continuationist reading of divine_marriage_command').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuationist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_tr_t0, continuationist_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(cont_tr_t30, continuationist_reading, theater_ratio, 30, 0.63).
narrative_ontology:measurement(cont_tr_t60, continuationist_reading, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(cont_be_t0, continuationist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(cont_be_t30, continuationist_reading, base_extractiveness, 30, 0.54).
narrative_ontology:measurement(cont_be_t60, continuationist_reading, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuationist_reading, identity_coordination).
narrative_ontology:affects_constraint(continuationist_reading, substitutionist_reading).
narrative_ontology:affects_constraint(continuationist_reading, coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% The divine_marriage_command kernel admits multiple readings. This constraint family decomposes into three stories corresponding to three coherent readings: continuationist_reading (this file, ε ≈ 0.58, Tangled Rope), substitutionist_reading (ε ≈ 0.15, Piton), and coercion_visibility_reading (ε ≈ 0.62, Snare). Each reading produces a different constraint because each instantiates a different interpretation of the Manifesto's status and therefore a different structure of obligations and extraction. The network links reflect that these constraints share a kernel but produce different ε values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuationist_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
