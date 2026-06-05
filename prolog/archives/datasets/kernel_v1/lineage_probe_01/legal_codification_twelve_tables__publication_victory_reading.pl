% ============================================================================
% CONSTRAINT STORY: legal_codification_twelve_tables__publication_victory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_codification_twelve_tables__publication_victory_reading, []).

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
 *   constraint_id: legal_codification_twelve_tables__publication_victory_reading
 *   human_readable: The Twelve Tables: Publication as Victory Over Priestly Monopoly
 *   domain: legal/doctrinal
 *
 * SUMMARY:
 *   The Tables' posting in the Forum c. 450 BCE represents a pivot in Roman
 *   legal history: the transition from law-as-secret-priestly-knowledge to
 *   law-as-public-text. This reading focuses on the publication act itself as
 *   the constraint's defining feature. Before codification, Roman legal
 *   procedure was opaque to plebeians: the pontiffs controlled the formulas,
 *   the procedural calendar, the interpretation of right and wrong. A
 *   plebeian defendant faced adjudication by rules they could not know, could
 *   not contest, could not prepare against. The pontiffs' power rested
 *   entirely on their monopoly over legal knowledge — their ability to say
 *   what the law was because no one else could verify or contradict them. The
 *   Tables' victory, in this reading, is strictly epistemological: by posting
 *   written law in public, the plebeians ended the monopoly on knowing what
 *   the law was. The pontiffs retained some authority (as expert interpreters
 *   of the written rules) but lost the power to adjudicate in secret. The
 *   constraint that operated through total suppression of alternatives (only
 *   the pontiffs could know) was replaced by a coordination mechanism:
 *   written law that all parties could read and appeal to. The extractiveness
 *   of the old order — the systematic extraction of plebeian resources and
 *   status through unknowable rules — ended.
 *
 * KEY AGENTS:
 *   - Plebeian Defendant/Litigant (powerless → powerful transition): Primary beneficiary. Before the Tables: trapped by knowledge monopoly, maximum extraction. After the Tables: can read the law, prepare defense, appeal to written rule.
 *   - Pontiff Class (institutional/constrained): Primary victim (of monopoly loss). Lose exclusive interpretive authority and the power to adjudicate in secret, but retain some coordinating role as experts in ritual and procedure.
 *   - Plebeian Assembly (organized/constrained): Collective agent that demanded and secured the codification. Achieved coordination solution to the collective action problem of legal opacity.
 *   - Patrician Class (institutional/arbitrage, not explicitly modeled): Secondary beneficiary. Retain power through the written rules while the plebeians gain knowledge access — codification serves patrician interests even as it empowers plebeians.
 *   - Analytical Observer (analytical/analytical): Civilizational view risks naturalizing the codification as inevitable development rather than as a politically contested victory wrested from the pontiffs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_codification_twelve_tables__publication_victory_reading, 0.18).
domain_priors:suppression_score(legal_codification_twelve_tables__publication_victory_reading, 0.72).
domain_priors:theater_ratio(legal_codification_twelve_tables__publication_victory_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_codification_twelve_tables__publication_victory_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(legal_codification_twelve_tables__publication_victory_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legal_codification_twelve_tables__publication_victory_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_codification_twelve_tables__publication_victory_reading, rope).
narrative_ontology:human_readable(legal_codification_twelve_tables__publication_victory_reading, "The Twelve Tables: Publication as Victory Over Priestly Monopoly").
narrative_ontology:topic_domain(legal_codification_twelve_tables__publication_victory_reading, "legal/doctrinal").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_codification_twelve_tables__publication_victory_reading, '71e73a44-7c2e-4d04-9af0-32061d6344fb').
narrative_ontology:cs_kernel_codification('71e73a44-7c2e-4d04-9af0-32061d6344fb', fixed_text).
narrative_ontology:cs_authority_grounding('71e73a44-7c2e-4d04-9af0-32061d6344fb', extraction).
narrative_ontology:cs_interpretation_layer_present('71e73a44-7c2e-4d04-9af0-32061d6344fb').
narrative_ontology:cs_reading_relation('71e73a44-7c2e-4d04-9af0-32061d6344fb', legal_codification_twelve_tables__foundation_myth_reading, coexists_with).
narrative_ontology:cs_reading_relation('71e73a44-7c2e-4d04-9af0-32061d6344fb', legal_codification_twelve_tables__harsh_content_reading, influences).
narrative_ontology:cs_axiom('71e73a44-7c2e-4d04-9af0-32061d6344fb', foundational, knowledge_monopoly_was_extraction_mechanism).
narrative_ontology:cs_axiom_status(knowledge_monopoly_was_extraction_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('71e73a44-7c2e-4d04-9af0-32061d6344fb', knowledge_monopoly_was_extraction_mechanism, deontological).
narrative_ontology:cs_axiom('71e73a44-7c2e-4d04-9af0-32061d6344fb', foundational, publication_ended_monopoly_efficacy).
narrative_ontology:cs_axiom_status(publication_ended_monopoly_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('71e73a44-7c2e-4d04-9af0-32061d6344fb', publication_ended_monopoly_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('71e73a44-7c2e-4d04-9af0-32061d6344fb', priestly_adjudicatory_monopoly).
narrative_ontology:cs_drift_state('71e73a44-7c2e-4d04-9af0-32061d6344fb', post_codification_forum_posting, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('71e73a44-7c2e-4d04-9af0-32061d6344fb', '').
narrative_ontology:cs_kernel_id(legal_codification_twelve_tables__publication_victory_reading, legal_codification_twelve_tables).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_codification_twelve_tables__publication_victory_reading, plebeian_litigants).
narrative_ontology:constraint_beneficiary(legal_codification_twelve_tables__publication_victory_reading, plebeian_citizen_body).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLEBEIAN DEFENDANT (SNARE) — Before the Tables, the defendant faced adjudication by secret rule: the pontiffs controlled the formulas, the calendar, the interpretive boundaries. The defendant could not know what law governed their case, could not prepare a defense against unknowable rules, and had no recourse if adjudication was arbitrary. Maximum extraction — the constraint operates entirely through suppression of alternatives and opacity.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PLEBEIAN LITIGANT AFTER CODIFICATION (ROPE) — Once the Tables are posted in the Forum, the litigant can read the law, can prepare a defense, and can appeal to written rule rather than priestly interpretation. The constraint now functions as pure coordination: the written law enables predictable adjudication and allows the litigant to structure their conduct within known bounds. Net benefit — the litigant's exit capacity is no longer suppressed by knowledge monopoly.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: PONTIFF CLASS (TANGLED ROPE) — The pontiffs lose their monopoly on legal interpretation but retain some coordination function: they are still the experts who can explain the written rules, still the custodians of ritual and procedure, still the authority that adjudicates disputes. The codification constrains them — they can no longer invent rules in secret — but they also benefit from the legitimacy that written law confers on their adjudicatory role. Mixed: loss of extractive monopoly power, retention of interpretive authority and social prestige.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: PLEBEIAN ASSEMBLY (ROPE) — The assembly that demanded the Tables accomplished coordination: they secured a written law that binds all parties equally and that can be read by anyone. The constraint functions as pure coordination mechanism from this perspective — the assembly's exit option is constrained (they cannot undo the Tables once they exist) but they perceive the constraint as empowering, not extractive. They see themselves as having solved a collective action problem.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the codification of law appears as an inevitable historical development: as societies grow complex, oral custom becomes insufficient, and written law emerges as the natural solution. From this view, the Tables are not a victory wrested from the pontiffs but rather an immutable structural requirement of any complex legal order. The constraint appears as inherent to legal development itself.
constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_codification_twelve_tables__publication_victory_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legal_codification_twelve_tables__publication_victory_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(legal_codification_twelve_tables__publication_victory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. In this reading, the Tables ended the extractive mechanism of adjudication by secret rule. The remaining extractiveness (0.18) reflects residual disparities in interpretive capacity and remaining procedural advantages held by those with legal expertise — but the core extraction mechanism (knowledge monopoly) has been eliminated. The measurement shows a sharp drop from 0.68 (pre-codification) to 0.18 (post-codification), indicating a genuine structural change. Suppression (0.72): High. Even after publication, suppression remains substantial because (a) literacy rates are low (only some plebeians can read the posted text); (b) interpretive expertise remains concentrated among trained jurists; (c) procedural access still requires knowledge of formulas and ritual; (d) the pontiffs can still block cases through procedural gatekeeping. The Tables reduce suppression from 0.90 (total monopoly) to 0.72 (partial monopoly) by removing the absolute bar to legal knowledge, but suppression does not fall to rope levels (≤0.35) because access remains constrained. Theater ratio (0.35): Moderate. The posting itself is performative — it is a victory ritual, a public display of law, a demonstration of plebeian power. But the Tables are not purely theater because they actually constrain adjudication: the written text can be cited, the rules can be followed, discretion is actually limited (though not eliminated). The theater ratio increases from 0.15 (pre-codification, when the performance was the secret knowledge itself) to 0.35 (post-codification, when the performance is the public posting but the constraint is real).
 *
 * PERSPECTIVAL GAP:
 *   The plebeian defendant before codification experiences pure snare (maximum extraction through knowledge monopoly, no exit). The plebeian litigant after codification experiences pure rope (coordination through written law, exit option restored). The pontiff class experiences tangled rope (lose monopoly but retain interpretive authority and prestige). The plebeian assembly experiences rope (achieve collective coordination goal). The analytical observer risks seeing mountain (inevitable historical development) and thereby naturalizing what is actually a political victory. The gap between snare and rope for the plebeian perspective is the definitive signature of this reading's truth claim: if the Tables genuinely ended the monopoly, the plebeian perspective should shift from snare to rope. If it did not (if the Tables were merely symbolic or if knowledge remained concentrated), the classification would remain snare or shift only to tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) flows from the structural relationship to the knowledge monopoly constraint. Before codification: plebeians are pure targets (d≈1.0, full victim). The pontiffs are pure beneficiaries (d≈0.0, full extraction). After codification: plebeians move toward symmetry (d≈0.50, both benefits and costs). The pontiffs retain beneficiary status but weakened (d≈0.30, reduced extraction). The plebeian assembly, as the agent that demanded the Tables, is framed as beneficiary of their own collective action. The analytical observer at civilizational scope (d≈0.72, the natural-law position) risks dissolving the political agency (the collective demand) into inevitable development.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by showing that codification genuinely changed the structural relationship: from extraction through knowledge monopoly to coordination through written law, with residual suppression. The rope classification is stable across multiple perspectives (litigant, assembly) once the monopoly is broken. The tangled rope for the pontiffs reflects genuine mixed effects: they lose extractive power but retain coordinating authority. The snare-to-rope transition for the plebeian perspective is the diagnostic test: if this reading is correct, that transition should be observable in historical records (change in litigation patterns, appeal to written text, reduction in arbitrary adjudication).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    publication_efficacy,
    'Did posting the Tables actually end the pontiffs'' monopoly, or did interpretive authority merely migrate from oral transmission to written exegesis?',
    'Historical analysis of post-codification litigation: frequency of appeals to plain text vs. priestly interpretation; emergence of professional jurists outside the priestly class; degree to which written law constrained adjudicatory discretion',
    'If monopoly truly ended: publication victory reading is supported (rope classification holds). If authority merely migrated: the constraint should be reclassified as tangled_rope or snare with higher theater ratio — the Tables became a new source of interpretive authority rather than ending authority itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publication_efficacy, empirical, 'Whether codification actually eliminated priestly monopoly or merely transformed it').

omega_variable(
    literacy_and_access,
    'How many plebeians could actually read the Tables? Did written law provide genuine access or merely symbolic enfranchisement?',
    'Literacy rate estimates for 5th-century Rome; analysis of how law was actually communicated (oral proclamation, authorized readers, legal professionals); examination of whether illiteracy created a new form of knowledge monopoly',
    'If literacy was widespread: publication victory reading holds (rope classification). If literacy was rare: the Tables provided psychological victory (theater) but not genuine access — should reclassify as piton or scaffold (temporary symbol of victory that was not yet structurally effective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(literacy_and_access, empirical, 'Actual extent of plebeian access to written law given literacy constraints').

omega_variable(
    reading_contest_sibling_ambiguity,
    'Is this reading''s core claim (publication ended monopoly) logically compatible with the harsh_content_reading (codification fixed the existing order''s cruelties) within a single framework?',
    'Examination of whether the Tables simultaneously (a) empower plebeians by ending knowledge monopoly AND (b) constrain them by codifying existing hierarchies and harsh rules. Can both be true at once?',
    'If compatible: the readings coexist_with each other (different parties emphasize different aspects). If incompatible: one reading forecloses the other — the victory of publication cannot simultaneously empower plebeians and lock them into written cruelty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_sibling_ambiguity, conceptual, 'Logical compatibility between publication victory and harsh content codings').

omega_variable(
    knowledge_monopoly_mechanism,
    'What was the actual mechanism of the pontiffs'' legal monopoly? Was it textual secrecy, interpretive authority, ritual gatekeeping, or combination?',
    'Philological and historical analysis of pre-codification legal practice; examination of what the Tables explicitly changed (accessibility, formula availability, procedural rules) vs. what remained under priestly control (interpretation, calendar, ritual)',
    'If monopoly was primarily textual (secret formulas): publication genuinely ended it (rope classification holds). If monopoly was primarily interpretive (only priests could explain law''s meaning): publication reduced but did not end it (tangled_rope may be more accurate). If monopoly was ritual-based (control of procedural timing): publication had limited effect (piton or snare may be more accurate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_monopoly_mechanism, empirical, 'The structural mechanism of pre-codification pontiff monopoly').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_codification_twelve_tables__publication_victory_reading, 0, 1).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_precondification, legal_codification_twelve_tables__publication_victory_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(theater_postcondification_immediate, legal_codification_twelve_tables__publication_victory_reading, theater_ratio, 1, 0.35).

% Extraction over time
narrative_ontology:measurement(extraction_precondification, legal_codification_twelve_tables__publication_victory_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(extraction_postcondification_immediate, legal_codification_twelve_tables__publication_victory_reading, base_extractiveness, 1, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(suppression_precondification, legal_codification_twelve_tables__publication_victory_reading, suppression_requirement, 0, 0.9).
narrative_ontology:measurement(suppression_postcondification_immediate, legal_codification_twelve_tables__publication_victory_reading, suppression_requirement, 1, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_codification_twelve_tables__publication_victory_reading, information_standard).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__publication_victory_reading, legal_codification_twelve_tables__foundation_myth_reading).
narrative_ontology:affects_constraint(legal_codification_twelve_tables__publication_victory_reading, legal_codification_twelve_tables__harsh_content_reading).

% DUAL FORMULATION NOTE:
% The legal codification kernel has three distinct readings, each constituting a separate constraint. publication_victory_reading focuses on the epistemic change (knowledge monopoly ended); harsh_content_reading focuses on the normative content codified (existing hierarchies fixed in writing); foundation_myth_reading focuses on the Tables' later symbolic role (ancestor cult more than working law). Each reading has its own extractiveness, suppression, and classification. They are siblings linked by the shared kernel, not reducible to one another.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
