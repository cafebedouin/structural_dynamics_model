% ============================================================================
% CONSTRAINT STORY: hebrew_vitality__liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_vitality__liturgical_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: hebrew_vitality__liturgical_reading
 *   human_readable: Liturgical Continuity as Constitutive of Hebrew Vitality
 *   domain: sociolinguistics/religious_studies/language_revitalization
 *
 * SUMMARY:
 *   This story instantiates the liturgical_reading of the contested
 *   hebrew_vitality kernel: the claim that unbroken liturgical use of Hebrew
 *   across the diaspora constitutes vitality in itself, rather than merely
 *   preserving material later drawn upon by a separate vernacular revival.
 *   This reading locates the kernel's occupying commitment squarely in ritual
 *   domain practice — synagogue recitation, textual transmission,
 *   cantillation — and treats that domain as sufficient rather than
 *   preparatory. It is deliberately NOT the native_daily_reading (which
 *   denies recitation counts as life at all) nor the
 *   hybrid_continuity_reading (which treats liturgical practice as necessary
 *   substrate but insufficient without reconstruction). Those are separate
 *   constraints with their own ε and stakeholder structures, linked here via
 *   network only.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: agenda-setting institutional beneficiary who administers liturgical correctness and derives continued relevance from the constitutive-vitality framing
 *   - synagogue_institutions: organized beneficiary whose communal centrality depends on liturgical practice being treated as sufficient
 *   - diaspora_practitioners: powerless payer/beneficiary who inherit continuity and belonging but may under-invest in vernacular fluency under this framing
 *   - vernacular_revival_advocates: excluded voice whose native-generation criterion is foreclosed by this reading's own kernel commitment
 *   - linguistic_historians: analytical observer describing what liturgical use empirically accomplished versus what it did not
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_vitality__liturgical_reading, 0.18).
domain_priors:suppression_score(hebrew_vitality__liturgical_reading, 0.22).
domain_priors:theater_ratio(hebrew_vitality__liturgical_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(hebrew_vitality__liturgical_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_vitality__liturgical_reading, rope).
narrative_ontology:human_readable(hebrew_vitality__liturgical_reading, "Liturgical Continuity as Constitutive of Hebrew Vitality").
narrative_ontology:topic_domain(hebrew_vitality__liturgical_reading, "sociolinguistics/religious_studies/language_revitalization").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_vitality__liturgical_reading, 'a672c665-43ec-45ba-acdb-c5c85b87131a').
narrative_ontology:cs_kernel_codification('a672c665-43ec-45ba-acdb-c5c85b87131a', fixed_text).
narrative_ontology:cs_authority_grounding('a672c665-43ec-45ba-acdb-c5c85b87131a', lineage).
narrative_ontology:cs_interpretation_layer_present('a672c665-43ec-45ba-acdb-c5c85b87131a').
narrative_ontology:cs_reading_relation('a672c665-43ec-45ba-acdb-c5c85b87131a', hebrew_vitality__native_daily_reading, forecloses).
narrative_ontology:cs_reading_relation('a672c665-43ec-45ba-acdb-c5c85b87131a', hebrew_vitality__hybrid_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('a672c665-43ec-45ba-acdb-c5c85b87131a', foundational, ritual_recitation_is_sufficient_for_vitality).
narrative_ontology:cs_axiom_status(ritual_recitation_is_sufficient_for_vitality, holdable).
narrative_ontology:cs_axiom_grounding('a672c665-43ec-45ba-acdb-c5c85b87131a', ritual_recitation_is_sufficient_for_vitality, conventional).
narrative_ontology:cs_axiom('a672c665-43ec-45ba-acdb-c5c85b87131a', secondary, textual_fidelity_across_generations_is_life).
narrative_ontology:cs_axiom_status(textual_fidelity_across_generations_is_life, holdable).
narrative_ontology:cs_axiom_grounding('a672c665-43ec-45ba-acdb-c5c85b87131a', textual_fidelity_across_generations_is_life, conventional).
narrative_ontology:cs_reference_frame('a672c665-43ec-45ba-acdb-c5c85b87131a', continuous_liturgical_transmission).
narrative_ontology:cs_drift_state('a672c665-43ec-45ba-acdb-c5c85b87131a', post_zionist_vernacular_revival, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a672c665-43ec-45ba-acdb-c5c85b87131a', '').
narrative_ontology:cs_kernel_id(hebrew_vitality__liturgical_reading, hebrew_vitality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, synagogue_institutions).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, liturgical_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_vitality__liturgical_reading, diaspora_practitioners).
narrative_ontology:constraint_victim(hebrew_vitality__liturgical_reading, diaspora_practitioners).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, sacred_language_persistence_doctrine).
narrative_ontology:constraint_vindicates(hebrew_vitality__liturgical_reading, textual_transmission_as_life).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the liturgical calendar, prayer texts, and standards of correct recitation that define what counts as legitimate Hebrew use in worship. Their communal authority and institutional relevance are bound up with the claim that unbroken liturgical practice is itself what kept Hebrew alive across the diaspora centuries. They set the framing that recitation-as-preservation constitutes vitality, and this framing is what makes their gatekeeping of liturgical correctness meaningful.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, rabbinic_authorities, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, rabbinic_authorities, beneficiary).

% Depend on continuous liturgical practice to justify their communal function and draw participation. A framing in which liturgical recitation alone constitutes vitality supports the institution's centrality; a framing that demotes recitation to mere preservation (requiring separate vernacular revival) would relativize the synagogue's role as the primary site of Hebrew's living continuation.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, synagogue_institutions, beneficiary,
    organized, generational, constrained, regional).

% Produce and distribute prayer books, cantillation guides, and liturgical Hebrew instructional material. Their market exists because liturgical Hebrew is treated as a discrete, sufficient domain of linguistic life rather than a fossilized ritual register awaiting separate revival.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, liturgical_publishers, beneficiary,
    moderate, generational, mobile, global).

% Recite liturgical Hebrew fluently in worship contexts, often without conversational competence in the language, and are told this recitation constitutes meaningful linguistic vitality. They inherit a sense of continuity and belonging from the practice, but bear the cost of a framing that can discourage investment in spoken fluency by treating ritual competence as already sufficient.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, diaspora_practitioners, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_vitality__liturgical_reading, diaspora_practitioners, beneficiary).

% Argue that liturgical recitation, however unbroken, is a preserved fossil rather than living language, and that only the deliberate 19th-20th century project of vernacular reconstruction (Ben-Yehuda and successors) produced actual vitality. Their claim is structurally excluded from the liturgical reading's own kernel, since admitting it would relocate the vitality claim outside ritual practice entirely.
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, vernacular_revival_advocates, excluded,
    moderate, generational, mobile, national).

% Study the historical record of Hebrew's status across two millennia of diaspora and can characterize what liturgical use actually accomplished (register maintenance, textual stability, phonological anchoring) versus what it did not accomplish (native acquisition, generative syntax, everyday communicative range).
narrative_ontology:constraint_stakeholder(hebrew_vitality__liturgical_reading, linguistic_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_vitality__liturgical_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(hebrew_vitality__liturgical_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unbroken liturgical recitation preserved a stable phonological, lexical, and textual register of Hebrew across dispersed communities for roughly two thousand years, providing continuity of religious practice and a shared linguistic anchor across otherwise disconnected Jewish communities.
% TRANSFER_FUNCTION: The arrangement moves communal authority and institutional legitimacy toward those who administer correct liturgical practice (rabbinic authorities, synagogues, liturgical publishers), and moves interpretive deference from lay practitioners toward those authorities, in exchange for the sense of continuity and belonging the practice provides.
% ABSENT_VOICES: Vernacular revival advocates and linguists who study native acquisition would object that this reading conflates preservation with vitality, effectively crediting ritual recitation with an achievement (living language status) it did not by itself produce. They are not part of the liturgical authority structure and their framing is not adjudicated within it.
% DISAPPEARANCE_RATIONALE: If liturgical Hebrew practice vanished, rabbinic authorities and synagogue institutions would lose a central claim to their historical role in Hebrew's continuity, and liturgical publishers would lose a market — the world of religious institutional authority would visibly rearrange. But whether the LANGUAGE's vitality would be affected is exactly the contested question the sibling readings dispute; under the native-daily reading, liturgical Hebrew's disappearance would not touch actual language vitality at all since that vitality is held to reside elsewhere.
% FOUNDING_PROBLEM: In the absence of a shared territory and continuous communal contact, diaspora communities needed a stable, mutually intelligible medium for religious practice, scriptural access, and communal identity that would not fragment along the lines of vernacular languages absorbed from host societies.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and synagogue institutions attest the founding problem remains live (liturgical continuity still binds dispersed communities). Independent linguistic historians and vernacular revival scholars attest that whatever problem liturgical practice solved, it did not by itself solve the separate problem of producing a spoken vernacular — that required the 19th-20th century reconstruction project, a corroboration from outside the liturgical-authority interest that complicates the liturgical reading's self-sufficiency claim.
narrative_ontology:disappearance_verdict(hebrew_vitality__liturgical_reading, contested).
narrative_ontology:founding_problem_status(hebrew_vitality__liturgical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_vitality__liturgical_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_vitality__liturgical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_vitality__liturgical_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_vitality__liturgical_reading_tests).
:- end_tests(hebrew_vitality__liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.18) because the constraint imposes essentially no material cost — liturgical practice is voluntary religious participation, not a resource-extracting arrangement, and no victim set is declared per the expected structural delta. Suppression is modest (0.22): the framing is more persuasive/institutional than coercive, though it can discourage vernacular investment by declaring the domain already sufficient. Theater ratio rises modestly over the interval (0.15 to 0.30) reflecting an observed drift in some communities toward liturgical performance emphasis (correct cantillation, textual fidelity) somewhat decoupled from communicative or generative linguistic function — a mild Goodhart signal worth tracking, though it does not rise to a level suggesting pure theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and synagogue institutions sit near the beneficiary end: their authority and relevance are constituted by the reading's truth, and they have durable exit/arbitrage options (institutional continuity regardless of outcome). Diaspora practitioners are closer to symmetric-to-payer: they receive genuine communal and identity benefits from liturgical continuity, but bear the opportunity cost of a framing that can substitute for vernacular investment. Vernacular revival advocates are excluded rather than extracted from — their objection is structurally foreclosed from the kernel's own terms, not priced into it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a stable shared medium across a fragmented diaspora) is genuinely contested as live vs. dead: rabbinic authorities treat it as still live and self-sufficient, while independent linguistic historians corroborate that a DIFFERENT problem (producing native generative competence) was solved by an entirely separate 19th-20th century project. This is not classified as mandatrophy outright because the liturgical function itself has not atrophied — it continues to do real work (textual transmission, communal cohesion) — but the CLAIM that this function alone constitutes vitality is the contested overreach the kernel dispute exists to adjudicate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_preservative_function,
    'Does liturgical recitation genuinely CONSTITUTE linguistic vitality, or does it PRESERVE material (phonology, lexicon, text) that a separate act of reconstruction later converted into vitality?',
    'Comparative sociolinguistic analysis: examine whether communities with strong unbroken liturgical practice but no vernacular revival exposure (e.g., isolated diaspora communities prior to 20th-century Hebrew revival) exhibited any spontaneous generative/native competence, versus communities where revival was deliberately engineered on top of the same liturgical substrate.',
    'If liturgical practice alone never produced generative competence anywhere without deliberate reconstruction, this reading''s core premise (recitation IS vitality) is undermined in favor of the hybrid_continuity_reading or native_daily_reading; if some liturgical communities show spontaneous vernacular emergence, the liturgical_reading''s self-sufficiency claim gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_preservative_function, empirical, 'Whether liturgical practice by itself is generative of vitality or merely preservative of substrate.').

omega_variable(
    beneficiary_capture_of_definition,
    'Is the liturgical_reading''s definition of vitality shaped by genuine linguistic/historical analysis, or by the institutional interest of rabbinic authorities and synagogue institutions in maximizing the significance of the practice they administer?',
    'Trace whether the constitutive-vitality claim originates in independent linguistic scholarship or primarily in apologetic/institutional literature produced by or for religious authorities; check for convergent independent corroboration.',
    'If the definition is substantially authority-shaped rather than independently corroborated, the reading functions partly as institutional self-legitimation layered onto a genuine but more modest preservation function — a false-summit-adjacent dynamic even though this is a rope/coordination reading rather than a mountain claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_definition, conceptual, 'Whether the vitality-constitution claim is independently grounded or beneficiary-shaped.').

omega_variable(
    kernel_framing_underdetermination,
    'Is ''liturgical use occupies the kernel'' the only defensible framing of what the hebrew_vitality kernel actually consists of, or does the kernel more plausibly consist of the underlying textual/phonological substrate that BOTH liturgical practice and vernacular revival separately drew upon?',
    'Compare the CS classification under a kernel framed as ''liturgical practice'' (this story) versus a kernel framed as ''the preserved textual/phonological substrate itself, independent of any particular mode of use'' — the latter framing might dissolve the liturgical/native dispute entirely by relocating the kernel one level down.',
    'Under the substrate framing, this reading and native_daily_reading would not be competitors at all but complementary claims about different downstream uses of a shared, more basic preserved object — changing this story''s coexists_with/forecloses relations and potentially its claimed_type from rope toward mountain (an inert substrate) with the readings as later-layered constructs.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is better located at the practice level (liturgy) or the substrate level (preserved textual/phonological material).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_vitality__liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_vitality__liturgical_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hebr_tr_t20, hebrew_vitality__liturgical_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(hebr_tr_t40, hebrew_vitality__liturgical_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(hebr_tr_t60, hebrew_vitality__liturgical_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement(hebr_tr_t80, hebrew_vitality__liturgical_reading, theater_ratio, 80, 0.27).
narrative_ontology:measurement(hebr_tr_t100, hebrew_vitality__liturgical_reading, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_vitality__liturgical_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hebr_be_t20, hebrew_vitality__liturgical_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(hebr_be_t40, hebrew_vitality__liturgical_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(hebr_be_t60, hebrew_vitality__liturgical_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(hebr_be_t80, hebrew_vitality__liturgical_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(hebr_be_t100, hebrew_vitality__liturgical_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(hebrew_vitality__liturgical_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_vitality__liturgical_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_vitality__liturgical_reading, 0.1).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__native_daily_reading).
narrative_ontology:affects_constraint(hebrew_vitality__liturgical_reading, hebrew_vitality__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three members of the hebrew_vitality constraint family, each authoring a distinct reading of what 'vitality' consists of and where the kernel sits: liturgical_reading (this file — ritual practice constitutes vitality, low ε, rabbinic-authority beneficiary), native_daily_reading (only native generative acquisition counts; recitation is demoted to mere preservation), and hybrid_continuity_reading (liturgical practice is necessary substrate but insufficient alone; vitality requires added deliberate reconstruction). The three share no single ε — each reading's beneficiary set, victim set (or absence), and extraction profile differ structurally, per the ε-invariance principle. They are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
