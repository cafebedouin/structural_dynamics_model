% ============================================================================
% CONSTRAINT STORY: latin_correctness__rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_latin_correctness__rupture_reading, []).

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
 *   constraint_id: latin_correctness__rupture_reading
 *   human_readable: Classical Latin Purity Standard (Rupture Reading)
 *   domain: intellectual/historical/philological
 *
 * SUMMARY:
 *   The rupture reading treats classical Latin as a fixed textual standard
 *   that must be recovered from ancient sources and enforced through
 *   emendation and teaching. Medieval Latin is read as corruption—a
 *   degradation caused by ignorance, exposure to barbarian languages, and
 *   abandonment of classical norms. This reading emerged powerfully during
 *   the Italian Renaissance (roughly 1350–1600) as humanist philologists
 *   rediscovered classical texts and used them as a hammer against medieval
 *   scholarship. The founding coordination problem—how to maintain Latin as a
 *   supra-regional scholarly language after Rome's political collapse—is
 *   reframed by this reading as requiring active restoration of the classical
 *   standard. Medieval solutions (adaptive linguistic change, functional
 *   extension) are retroactively invalidated as failures. The reading is
 *   authoritative in universities by 1600 and shapes scholarly prestige for
 *   centuries. High extractiveness (0.72) reflects that the constraint
 *   transfers authority from medieval scholars to classical authorities and
 *   requires constant enforcement. Suppression is high (0.68) because the
 *   constraint must actively prevent alternatives—other readings of Latin
 *   usage, functional innovation, and the revaluation of medieval work—from
 *   being recognized as legitimate. Theater is moderate (0.48): some
 *   enforcement is real (genuine philological rigor), but increasing shares
 *   of the activity are performative (elaborate denigration of medieval texts
 *   to prove one's classical superiority, competitive display of purity
 *   mastery). The measurement series tracks the rise of the constraint from
 *   1300 (early humanist recovery) through 1650 (post-disciplinary
 *   consolidation). Theater_ratio rises to 0.42 by 1475 and plateaus at 0.48
 *   by 1550, indicating that by the time classical standards are
 *   institutionalized, much of the enforcement is rhetorical maintenance
 *   rather than active functional correction.
 *
 * KEY AGENTS:
 *   - Classical philologists: set the standard through textual scholarship; define correctness; control prestige
 *   - Humanist grammarians: build careers on recovery and emendation; gain authority by demonstrating classical mastery
 *   - Medieval scholars: retroactively delegitimized; their works are now read as corrupt rather than as adaptive
 *   - Technical domain practitioners: physicians, lawyers, theologians whose specialized Latin departs from classical norms and is thus marked as degraded
 *   - Vernacular-adjacent Latin users: clerics and scribes learning Latin without native-speaker immersion; identity-locked into a language they cannot master to classical standards
 *   - University authorities: enforce the standard through curricula and hiring; require classical mastery as mark of education
 *   - Roman restoration ideology: a vindicated proposition that classical Rome represents unrepeatable peak; benefits from constraint's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(latin_correctness__rupture_reading, 0.72).
domain_priors:suppression_score(latin_correctness__rupture_reading, 0.68).
domain_priors:theater_ratio(latin_correctness__rupture_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(latin_correctness__rupture_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(latin_correctness__rupture_reading, tangled_rope).
narrative_ontology:human_readable(latin_correctness__rupture_reading, "Classical Latin Purity Standard (Rupture Reading)").
narrative_ontology:topic_domain(latin_correctness__rupture_reading, "intellectual/historical/philological").

domain_priors:requires_active_enforcement(latin_correctness__rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(latin_correctness__rupture_reading, '5a77d170-f1d6-4569-934c-17448326e95b').
narrative_ontology:cs_kernel_codification('5a77d170-f1d6-4569-934c-17448326e95b', fixed_text).
narrative_ontology:cs_authority_grounding('5a77d170-f1d6-4569-934c-17448326e95b', lineage).
narrative_ontology:cs_interpretation_layer_present('5a77d170-f1d6-4569-934c-17448326e95b').
narrative_ontology:cs_reading_relation('5a77d170-f1d6-4569-934c-17448326e95b', latin_correctness__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5a77d170-f1d6-4569-934c-17448326e95b', latin_correctness__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('5a77d170-f1d6-4569-934c-17448326e95b', foundational, classical_purity_recoverable).
narrative_ontology:cs_axiom_status(classical_purity_recoverable, holdable).
narrative_ontology:cs_axiom_grounding('5a77d170-f1d6-4569-934c-17448326e95b', classical_purity_recoverable, empirically_contingent).
narrative_ontology:cs_axiom('5a77d170-f1d6-4569-934c-17448326e95b', foundational, medieval_departure_constitutes_degradation).
narrative_ontology:cs_axiom_status(medieval_departure_constitutes_degradation, holdable).
narrative_ontology:cs_axiom_grounding('5a77d170-f1d6-4569-934c-17448326e95b', medieval_departure_constitutes_degradation, deontological).
narrative_ontology:cs_axiom('5a77d170-f1d6-4569-934c-17448326e95b', secondary, ancient_authors_unsurpassable_standard).
narrative_ontology:cs_axiom_status(ancient_authors_unsurpassable_standard, holdable).
narrative_ontology:cs_axiom_grounding('5a77d170-f1d6-4569-934c-17448326e95b', ancient_authors_unsurpassable_standard, deontological).
narrative_ontology:cs_reference_frame('5a77d170-f1d6-4569-934c-17448326e95b', classical_roman_linguistic_perfection).
narrative_ontology:cs_drift_state('5a77d170-f1d6-4569-934c-17448326e95b', post_renaissance_institutional_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5a77d170-f1d6-4569-934c-17448326e95b', '').
narrative_ontology:cs_kernel_id(latin_correctness__rupture_reading, latin_correctness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, classical_philologists).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, humanist_grammarians).
narrative_ontology:constraint_beneficiary(latin_correctness__rupture_reading, roman_restoration_ideology).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, medieval_scholars).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, technical_domain_practitioners).
narrative_ontology:constraint_victim(latin_correctness__rupture_reading, vernacular_adjacent_latin_users).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(latin_correctness__rupture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(latin_correctness__rupture_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(latin_correctness__rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(latin_correctness__rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(latin_correctness__rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the constraint redistributes authority and prestige from medieval practitioners to classical scholars, and requires ongoing cognitive and institutional work to maintain. The beneficiaries (humanist scholars, university authorities, the ideology of classical supremacy) gain institutional power, prestige, and control over what counts as legitimate knowledge. The victims (medieval scholars, technical practitioners, vernacular-adjacent users) lose authority over their own intellectual traditions and face constant correction. Suppression is high because the constraint must actively prevent the recognition of medieval Latin as a legitimate functional register or adaptive solution. If medieval practice were allowed to stand as a valid reading of Latin history—as continuous development rather than corruption—the entire authority structure collapses. The constraint's persistence depends on suppressing the coherence of medieval linguistic work. Theater_ratio rises over the interval because by 1550, the classical standard is so well established in universities that much enforcement activity becomes rhetorical: competitive display of erudition, elaborate critique of medieval texts to prove one's superiority, performative distance-taking from the medieval world. The ratio plateaus at 0.48 because some genuine correction remains (authentic philological practice) but is increasingly overshadowed by performative maintenance. Accessibility_collapse is high (0.78 on-interval-end) because once the rupture reading is established, alternatives collapse: medieval usage is unthinkable as legitimate, functional innovation is marked as deviation, living adaptation is read as ignorance. The leveled coercion grid shows that suppression machinery rises most rapidly at the organizational level (universities, cathedral schools, intellectual networks) and somewhat less intensely at the individual level, where scribes and local practitioners retain some practical autonomy. Resistance is highest at the individual and class levels early (1300), as medieval practitioners and communities maintain their functional linguistic practices, and decays over the interval as the constraint's institutional embedding grows.
 *
 * PERSPECTIVAL GAP:
 *   The classical philologist and the medieval scholar experience this constraint in opposite ways. From the philologist's seat, the constraint is genuine coordination—recovery of a shared standard that enables scholarship across regions and centuries, restoration of access to the greatest minds of antiquity, and elevation of intellectual rigor. From the medieval scholar's seat, the constraint is enforced extraction—retroactive invalidation of their entire intellectual tradition, denial of legitimacy to their necessary linguistic innovations, and systematic denigration masquerading as correction. The engine should compute different types at these two seats: from the philologist's position the constraint may appear as rope or even mountain (recovered standard, natural authority of antiquity); from the medieval scholar's position it is clearly snare (coercive delegitimation, suppression of alternative validities, trapped target). University authorities occupy an intermediate position: they benefit from the constraint's existence (it simplifies curriculum, provides a clear standard for certification) but are somewhat mobile (they could in principle adopt a different standard) and face pushback from technical domains whose practitioners need functional flexibility. The breakdown should show institutional seats as partial beneficiaries and practical-domain seats as near-pure targets.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical philologists (institutional power) are the structural beneficiaries: they define correctness, control prestige, and benefit from the authority differential between themselves and everyone else. Humanist grammarians are similarly positioned (institutional, powerful, mobile exit—they can arbitrage their classical knowledge across multiple contexts). Medieval scholars are the primary victims: moderate power, constrained exit (they cannot simply cease being medieval scholars), biographical horizon (their works are fixed in time and cannot be revised to meet new standards). Technical practitioners are victims: moderate power, constrained exit (they need Latin for professional work and cannot abandon the domains where correctness standards are enforced), biographical horizon. Vernacular-adjacent users are victims at the powerless level: their exit is identity_locked (they are clerics, notaries, scribes—these roles require Latin), they have constrained alternatives (they cannot escape the context where classical standards are enforced), biographical horizon (they live with the constraint across their working lives). University authorities are agenda-setters: institutional power, they set the standard through hiring and curricula, but they are somewhat dependent on the broader intellectual authority structure (if the classical standard collapsed, their authority would shift). Church authorities are excluded: institutional power, but they have little voice in determining whether medieval ecclesiastical Latin should be revalued. The directionality profile should show: philologists d ≈ 0.1 (beneficiaries, arbitrage mobility), medieval scholars d ≈ 0.85 (trapped targets), technical practitioners d ≈ 0.75 (constrained targets), vernacular users d ≈ 0.9 (identity-locked targets), university authorities d ≈ 0.4 (partial beneficiaries with some vulnerability).
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading is a clear case of a constraint whose founding mandate has atrophied but whose enforcement persists. The founding problem was: 'How do we maintain Latin as a supra-regional scholarly language when it is no longer natively spoken?' By 1450, this problem is substantially solved through medieval innovation: Latin IS a functional supra-regional language; scholars across Europe communicate reliably in medieval registers. The constraint does not solve this problem—medieval practice already has. Instead, the constraint retroactively reframes the problem as 'How do we force current and future scholars to express themselves in the style of authors 1,500 years dead?' This is not the original mandate. Medieval scholars, university authorities, and technical practitioners all attest that the founding mandate is dead or substantially addressed. Classical philologists and humanist grammarians insist the mandate is live: without constant recovery and enforcement of classical purity, the language would degrade. But this restatement of the mandate is itself circular: the only sense in which the language would degrade is if it changed from its classical forms—which is exactly what happened naturally and continuously for 1,000 years without harm. The constraint persists as mandatrophy because it benefits the institutional class (scholars, universities) who now have a permanent justification for their authority: the eternal task of correcting medieval ignorance and training students in classical mastery. The theater_ratio rise from 0.25 (1300) to 0.48 (1650) tracks exactly this mandatrophy: as the actual functional problem (maintaining supra-regional Latin) is solved, the visible activity becomes increasingly performative—correction, emendation, critique, and display of superior erudition—rather than functional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classical_standard_fixity,
    'Is classical Latin a fixed standard that can be recovered from texts, or was it itself a living, diverse, internally variable phenomenon that texts only partially represent?',
    'Linguistic analysis of classical texts showing internal variation, regional dialects, register distinctions, and evolution across classical centuries; comparison with how much medieval variation represents comparable linguistic phenomena versus genuine corruption.',
    'If classical Latin was internally variable, the rupture reading''s core claim (there is a single standard to recover and maintain) collapses. Medieval variations would represent normal linguistic change rather than degradation. The entire authority structure shifts from enforcement of purity to acceptance of diversity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classical_standard_fixity, empirical, 'Whether classical Latin was a fixed monolith or an internally diverse linguistic reality').

omega_variable(
    medieval_functionality_necessity,
    'Were medieval departures from classical norms necessary adaptations to express new concepts (Christian theology, Germanic legal forms, technical innovation), or could these concepts have been expressed in classical forms if medieval scholars had tried harder?',
    'Conceptual analysis and historical case studies: attempt to express Christian theological concepts (transubstantiation, sacramental theology, the Trinity) in purely classical Ciceronian forms, and document whether the result preserves meaning or requires neo-classical extension. Analyze whether technical domains (medicine, law, liturgy) genuinely required new Latin vocabulary.',
    'If medieval extensions were necessary, they are adaptive innovation rather than corruption, and the victims'' exit constraint is illusory—they did the best possible with the constraints they faced. This reframes the constraint from enforcement of standards to extraction through retroactive invalidation. If extensions were merely convenient rather than necessary, the rupture reading''s evaluation becomes defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_functionality_necessity, conceptual, 'Whether medieval linguistic extensions were functional necessities or avoidable departures').

omega_variable(
    authority_extraction_versus_coordination,
    'Does the rupture reading''s enforcement primarily serve to maintain a real coordination function (unified scholarly language), or primarily to maintain the authority and prestige hierarchy of classical scholars over medieval scholars?',
    'Comparative analysis: measure how much of the enforcement machinery targets functional clarity (shared vocabulary, reliable meaning) versus symbolic purity (rejection of medieval forms, emendation of texts, denigration of medieval scholarship). Analyze whether technical practitioners achieve better functional results by following classical norms or by adopting medieval technical registers.',
    'If coordination is primary, the extraction is a side effect of necessary enforcement. If authority maintenance is primary, the constraint is pure snare dressed in coordination language. The theater_ratio rise from 0.25 to 0.48 suggests increasing separation of performative activity from functional necessity, supporting the extraction hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_extraction_versus_coordination, empirical, 'Whether enforcement serves coordination or primarily maintains authority hierarchy').

omega_variable(
    alternative_kernel_readings,
    'Could the latin_correctness kernel be stable under the continuity_reading or hybrid_reading instead of the rupture reading, with comparable functional outcomes?',
    'Counterfactual historical analysis: if humanist scholars had adopted the continuity reading (medieval Latin is legitimate development) in 1450, would the supra-regional function of Latin have been preserved? Would scholarship have been less clear or coherent? What institutional and prestige outcomes would differ?',
    'If alternative readings would be functionally equivalent, the rupture reading''s dominance reflects extractive power concentration rather than genuine necessity. If the rupture reading provides superior coordination, its enforcement is defensible. The mandate-atrophy diagnosis suggests functional equivalence, implying the constraint persists as extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_kernel_readings, conceptual, 'Whether the rupture reading is functionally superior or merely dominant due to power concentration').

omega_variable(
    kernel_authority_lineage,
    'What is the source of the rupture reading''s authority? Does it derive from recovery of classical practice itself, or from contemporary commitments to Romanitas (restoration ideology) that retroactively impose the reading onto the classical texts?',
    'Historical genealogy: trace the development of the rupture reading from early humanist scholars (Petrarch, Valla) through institutional consolidation. Distinguish between (a) the reading arising from close study of classical texts revealing what classicality was, and (b) the reading being produced by applying a prior ideology of Roman supremacy to the texts and then claiming the texts generated the reading.',
    'If (a) is true, the rupture reading derives its authority from the classical texts themselves. If (b) is true, the reading''s authority derives from an ideology, and the texts are being used instrumentally to legitimize extraction. This affects whether the constraint is a genuine recovery or a performative legitimation of a power shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_authority_lineage, conceptual, 'Whether the rupture reading''s authority comes from classical practice or from contemporary restoration ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(latin_correctness__rupture_reading, 1300, 1650).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lati_tr_t1300, latin_correctness__rupture_reading, theater_ratio, 1300, 0.25).
narrative_ontology:measurement(lati_tr_t1400, latin_correctness__rupture_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(lati_tr_t1475, latin_correctness__rupture_reading, theater_ratio, 1475, 0.42).
narrative_ontology:measurement(lati_tr_t1550, latin_correctness__rupture_reading, theater_ratio, 1550, 0.47).
narrative_ontology:measurement(lati_tr_t1600, latin_correctness__rupture_reading, theater_ratio, 1600, 0.48).
narrative_ontology:measurement(lati_tr_t1650, latin_correctness__rupture_reading, theater_ratio, 1650, 0.48).

% Extraction over time
narrative_ontology:measurement(lati_be_t1300, latin_correctness__rupture_reading, base_extractiveness, 1300, 0.45).
narrative_ontology:measurement(lati_be_t1400, latin_correctness__rupture_reading, base_extractiveness, 1400, 0.58).
narrative_ontology:measurement(lati_be_t1475, latin_correctness__rupture_reading, base_extractiveness, 1475, 0.65).
narrative_ontology:measurement(lati_be_t1550, latin_correctness__rupture_reading, base_extractiveness, 1550, 0.7).
narrative_ontology:measurement(lati_be_t1600, latin_correctness__rupture_reading, base_extractiveness, 1600, 0.71).
narrative_ontology:measurement(lati_be_t1650, latin_correctness__rupture_reading, base_extractiveness, 1650, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(lati_su_t1300, latin_correctness__rupture_reading, suppression_requirement, 1300, 0.4).
narrative_ontology:measurement(lati_su_t1400, latin_correctness__rupture_reading, suppression_requirement, 1400, 0.52).
narrative_ontology:measurement(lati_su_t1475, latin_correctness__rupture_reading, suppression_requirement, 1475, 0.62).
narrative_ontology:measurement(lati_su_t1550, latin_correctness__rupture_reading, suppression_requirement, 1550, 0.66).
narrative_ontology:measurement(lati_su_t1600, latin_correctness__rupture_reading, suppression_requirement, 1600, 0.67).
narrative_ontology:measurement(lati_su_t1650, latin_correctness__rupture_reading, suppression_requirement, 1650, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(latin_correctness__rupture_reading, information_standard).
narrative_ontology:boltzmann_floor_override(latin_correctness__rupture_reading, 0.12).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__continuity_reading).
narrative_ontology:affects_constraint(latin_correctness__rupture_reading, latin_correctness__hybrid_reading).

% DUAL FORMULATION NOTE:
% The latin_correctness kernel decomposes into three structurally distinct constraint stories, one per reading of what counts as correct Latin. The rupture_reading treats classical Latin as a fixed standard requiring recovery and enforcement; the continuity_reading treats medieval Latin as legitimate organic development; the hybrid_reading accepts classical norms in prestige domains while legitimizing medieval forms in technical domains. These are not three perspectives on one constraint—they are three incompatible constraint structures instantiated by the same kernel under different readings. Each has its own ε, victim/beneficiary set, and type. The rupture reading is the most extractive (0.72), most suppressive (0.68), and most dependent on active enforcement. The continuity reading would show minimal extraction, no designated victims, and mountain-type characteristics (medieval Latin is just what happened naturally). The hybrid reading would show medium extraction in prestige domains, technical practitioners as beneficiaries, and tangled-rope type (genuine functional division of labor, but authority extracted from technical domains). All three readings coexist in contemporary scholarship; no single reading has achieved foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(latin_correctness__rupture_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
