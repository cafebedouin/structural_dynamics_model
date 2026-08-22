% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__exclusive_inspiration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__exclusive_inspiration_reading, []).

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
 *   constraint_id: kjv_text_1611__exclusive_inspiration_reading
 *   human_readable: KJV Exclusive Inspiration Doctrine (Reading)
 *   domain: religious/textual
 *
 * SUMMARY:
 *   The KJV-Only doctrine claims that the King James Version is the
 *   exclusively inspired, inerrant English Bible, and that all other
 *   translations are corrupted or inferior. This constraint story
 *   instantiates ONE READING of the contested kernel 'the KJV text of
 *   1611'—specifically, the exclusive-inspiration reading. Other readings
 *   exist (a functional-equivalence reading that values multiple
 *   translations, a revisable-translation reading that treats the KJV as a
 *   historically important but improvable text). This story describes ONLY
 *   the exclusive-inspiration reading and the structural extraction it
 *   enables: it gates interpretive authority to KJV-Only leadership,
 *   suppresses modern translations and their publishers, and traps believers
 *   in a regime where textual uncertainty is reframed as personal spiritual
 *   failure. The extractiveness is high and rising over the interval as
 *   institutional suppression mechanisms harden and alternative texts are
 *   increasingly categorized as actively Satanic rather than merely inferior.
 *   The measurement series tracks both the base extractiveness (the direct
 *   extraction of authority) and the suppression requirement (the active
 *   enforcement machinery needed to keep believers from consulting modern
 *   translations).
 *
 * KEY AGENTS:
 *   - KJV-Only leadership: organized institutional actors (regional networks of fundamentalist churches, publishing houses, conference organizers) that set and enforce the doctrine. They directly benefit from exclusive textual authority.
 *   - Pulpit gatekeepers: individual pastors in KJV-Only congregations whose professional standing depends on the doctrine. They are both beneficiaries (authority) and enforcers.
 *   - Modern translation publishers: powerful institutional actors (Zondervan, Crossway, etc.) whose work is systematically suppressed and delegitimized by the doctrine.
 *   - Evangelical scholars: textual critics and biblical linguists whose scholarly work contradicts the doctrine and who face institutional exclusion as a result.
 *   - Ordinary believers: powerless individuals in KJV-Only congregations who bear the cost of the doctrine (difficulty understanding archaic language, intellectual suppression, spiritual manipulation) and have no exit.
 *   - Manuscript evidence: the analytical outsider—earlier Greek texts that predate the KJV by 1,500 years and would justify revision. The doctrine's persistence requires this evidence to be systematically excluded from consideration.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, 0.78).
domain_priors:suppression_score(kjv_text_1611__exclusive_inspiration_reading, 0.81).
domain_priors:theater_ratio(kjv_text_1611__exclusive_inspiration_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(kjv_text_1611__exclusive_inspiration_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__exclusive_inspiration_reading, snare).
narrative_ontology:human_readable(kjv_text_1611__exclusive_inspiration_reading, "KJV Exclusive Inspiration Doctrine (Reading)").
narrative_ontology:topic_domain(kjv_text_1611__exclusive_inspiration_reading, "religious/textual").

domain_priors:requires_active_enforcement(kjv_text_1611__exclusive_inspiration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__exclusive_inspiration_reading, '8cdfd02e-e68d-49ae-a112-a193c7599e01').
narrative_ontology:cs_kernel_codification('8cdfd02e-e68d-49ae-a112-a193c7599e01', fixed_text).
narrative_ontology:cs_authority_grounding('8cdfd02e-e68d-49ae-a112-a193c7599e01', extraction).
narrative_ontology:cs_interpretation_layer_present('8cdfd02e-e68d-49ae-a112-a193c7599e01').
narrative_ontology:cs_reading_relation('8cdfd02e-e68d-49ae-a112-a193c7599e01', kjv_text_1611__functional_equivalence_reading, coexists_with).
narrative_ontology:cs_reading_relation('8cdfd02e-e68d-49ae-a112-a193c7599e01', kjv_text_1611__revisable_translation_reading, influences).
narrative_ontology:cs_axiom('8cdfd02e-e68d-49ae-a112-a193c7599e01', foundational, kjv_divine_inspiration_exclusive).
narrative_ontology:cs_axiom_status(kjv_divine_inspiration_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('8cdfd02e-e68d-49ae-a112-a193c7599e01', kjv_divine_inspiration_exclusive, deontological).
narrative_ontology:cs_axiom('8cdfd02e-e68d-49ae-a112-a193c7599e01', secondary, alternative_translations_corrupted).
narrative_ontology:cs_axiom_status(alternative_translations_corrupted, holdable).
narrative_ontology:cs_axiom_grounding('8cdfd02e-e68d-49ae-a112-a193c7599e01', alternative_translations_corrupted, empirically_contingent).
narrative_ontology:cs_reference_frame('8cdfd02e-e68d-49ae-a112-a193c7599e01', divinely_preserved_english_text).
narrative_ontology:cs_drift_state('8cdfd02e-e68d-49ae-a112-a193c7599e01', contemporary_evangelical_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8cdfd02e-e68d-49ae-a112-a193c7599e01', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:constraint_beneficiary(kjv_text_1611__exclusive_inspiration_reading, pulpit_gatekeepers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, modern_translation_publishers).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, evangelical_scholars).
narrative_ontology:constraint_victim(kjv_text_1611__exclusive_inspiration_reading, ordinary_believers_seeking_clarity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A network of fundamentalist churches, ministries, and publishing houses (e.g., Bob Jones University Press, certain Pentecostal denominations, independent Baptist networks) that have made the KJV-Only doctrine central to institutional identity. They control which translations are promoted from pulpits, which commentaries are stocked in bookstores, which speakers are invited to conferences. They set the terms of textual authority and enforce compliance through institutional exclusion. They directly benefit: their interpretations become doctrine, their publishing materials sell to believers dependent on their gatekeeping, their leadership is unchallenged.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership, agenda_setter,
    organized, generational, identity_locked, regional).

% Individual pastors and Bible teachers in KJV-Only congregations whose authority rests on exclusive access to the 'true' text. They preach that the KJV is inerrant while other translations are corrupted. Their congregations trust them as textual authorities precisely because the doctrine reserves textual legitimacy to those who know the KJV is superior. Switching to modern translations risks losing congregational trust, professional standing, and coherence within their theological framework. They benefit from the doctrine by maintaining unchallenged authority.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, pulpit_gatekeepers, beneficiary,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__exclusive_inspiration_reading, pulpit_gatekeepers, agenda_setter).

% Major publishers (Zondervan, Crossway, Tyndale House, Bible Gateway, etc.) that produce modern translations (NIV, ESV, NRSV, NCV, etc.) marketed for readability and manuscript accuracy. Under the exclusive-inspiration reading they are positioned as merchants of corrupted scripture. Their translations are banned from KJV-Only institutions, their names are used from pulpits as examples of apostasy, their scholarship is dismissed as unregenerate rationalism. They face suppression through rhetorical delegitimization and institutional exclusion, though they have mobile exit (they publish to the much larger evangelical mainstream).
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, modern_translation_publishers, payer,
    powerful, generational, mobile, global).

% Textual critics, biblical scholars, and linguists (e.g., scholars at evangelical seminaries, universities like Wheaton or Biola, Christianity Today contributors) who have used earlier manuscripts (Dead Sea Scrolls, P45, Codex Sinaiticus) and comparative linguistics to produce and defend modern translations. Their scholarship contradicts the KJV-Only doctrine. They face institutional suppression: they cannot speak at KJV-Only conferences, cannot publish in KJV-Only venues, cannot teach in KJV-Only seminaries without renouncing their scholarly conclusions. Their exit is constrained—they could leave scholarship but then lose professional identity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, evangelical_scholars, payer,
    powerful, biographical, constrained, national).

% Church members in KJV-Only congregations (adults and youth) who struggle with the archaic English of the KJV (thee/thou, -eth verbs, obsolete words like 'holpen') and would benefit from clearer modern translations. They are taught that their difficulty understanding the text is a personal spiritual failure, not a translation clarity issue. They are discouraged from consulting modern translations, condemned if caught reading the NIV or ESV, told that the Holy Spirit will make the KJV plain if their faith is genuine. They bear the cost of intellectual suppression and spiritual manipulation to remain in congregational good standing. They are trapped: exiting the congregation means losing community, family, faith identity.
narrative_ontology:constraint_stakeholder(kjv_text_1611__exclusive_inspiration_reading, ordinary_believers_seeking_clarity, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kjv_text_1611__exclusive_inspiration_reading, kjv_only_leadership).
narrative_ontology:fixing_cost_class(kjv_text_1611__exclusive_inspiration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The exclusive-inspiration reading does not solve a coordination problem. It asserts a truth claim (the KJV is uniquely inspired) and uses that claim to justify institutional gatekeeping. A rope or tangled rope would solve a collective-action challenge (e.g., 'how do we agree on a single authoritative text so translation disputes don't fragment the church?'). The exclusive-inspiration reading does not solve that—it asserts one answer and suppresses all others. No genuine coordination problem exists that this doctrine solves.
% TRANSFER_FUNCTION: Moves interpretive authority, publishing revenue, ministerial standing, and spiritual legitimacy from distributed scholarly consensus (evangelical scholars, modern translation publishers) to concentrated gate-keeper institutions (KJV-Only leadership, pulpits). Believers transfer their intellectual assent—their acceptance of the doctrine as revealed truth—to institutional gatekeepers in exchange for spiritual assurance that they have the 'true' Bible. The transfer is asymmetric: gatekeepers gain authority and revenue; payer seats lose markets, credibility, and institutional access.
% ABSENT_VOICES: Textual scholars are structurally excluded: their evidence contradicts the doctrine and is therefore categorized as unregenerate rationalism or demonic deception, pre-emptively delegitimizing any contribution they might make. Ordinary believers struggling with archaic language are also effectively excluded—their experience of difficulty is reframed as personal spiritual failure rather than a legitimate translation clarity issue. Modern translation publishers cannot participate in KJV-Only decision-making about textual authority. These absences are not incidental; they are engineered by the doctrine itself, which classifies contradictory evidence and voices as Satanic.
% DISAPPEARANCE_RATIONALE: If the KJV exclusive-inspiration doctrine disappeared overnight, modern translations would immediately re-enter pulpits and study groups. Evangelical scholars would face no institutional suppression for their textual work. Believers would gain access to clearer English texts suited to their reading level. Bible publishing would diversify freely without gatekeeping pressure. Ministerial authority would no longer rest on claims of exclusive access to the true text. The constraint's disappearance would reorganize authority over textual meaning from a concentrated gate-keeper class to a distributed scholarly consensus grounded in manuscript evidence and linguistic accessibility.
% FOUNDING_PROBLEM: In the 1600s-1700s, the KJV was the most reliable English translation available, produced by the best scholars of its era, and served as a stabilizing anchor for Protestant identity and doctrinal authority. In the 1920s-1950s, as higher criticism questioned traditional authorship and textual authority, the KJV-Only doctrine crystallized as a defensive claim: at least one English text had remained pure and incorruptible even as the academy abandoned biblical authority. This was meant to preserve textual stability in a world of theological fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: The KJV-Only movement itself acknowledges that modern biblical scholarship has established earlier and more reliable manuscripts than those underlying the KJV (they argue these newer manuscripts are corrupted; textual scholars outside the movement provide contrary evidence that these earlier manuscripts are more reliable). Evangelical scholars, textual critics, and Bible publishers attest that the founding problem—lack of a stable, reliable, understandable English translation—was solved 50+ years ago by modern critical scholarship and the production of multiple reliable modern versions. The doctrine persists not because the founding problem is live but because institutional gatekeepers benefit economically and socially from exclusive authority over textual legitimacy. Even scholars sympathetic to the KJV as literature attest it is not uniquely inspired—it is a good translation from a particular time, now superseded by better ones.
narrative_ontology:disappearance_verdict(kjv_text_1611__exclusive_inspiration_reading, world_rearranges).
narrative_ontology:founding_problem_status(kjv_text_1611__exclusive_inspiration_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__exclusive_inspiration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__exclusive_inspiration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__exclusive_inspiration_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_text_1611__exclusive_inspiration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kjv_text_1611__exclusive_inspiration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the doctrine transfers interpretive authority, publishing revenue, and institutional standing from distributed scholarly consensus to a concentrated gate-keeper class. The transfer is enforced not by law but by anathema: texts labeled as Satanic corruptions, publishers as merchants of false scripture, scholars as unregenerate rationalists. Suppression is high (0.81) because the doctrine's persistence requires actively preventing believers from consulting alternatives. This suppression is partly internalized (believers accept that struggling to understand archaic language is personal spiritual failure) and partly structural (institutional rules against modern translations). Theater is moderate (0.42): the doctrine rests partly on genuine theological conviction (that revelation is preserved in a single text) and partly on institutional protection of gatekeeping privilege. The measurement series shows extractiveness and suppression both rising over the interval as institutional gatekeepers respond to erosion from evangelical scholarship and Bible publication diversity by hardening suppression mechanisms. Suppression reaches a ceiling around t=25 (institutional capacity for enforcement maxes out), while extractiveness continues to rise as the doctrine's rentier benefits consolidate. The rising theater ratio indicates that over time, more enforcement activity is devoted to defending the gate itself (preventing alternative translations) rather than the original coordination function (providing a stable English text).
 *
 * PERSPECTIVAL GAP:
 *   From the KJV-Only leadership seat, the doctrine is seen as defense of textual purity and spiritual truth—a coordination function that keeps believers from being misled by false translations. From the evangelical scholar seat, the doctrine is pure extraction: institutional suppression and intellectual gatekeeping that prevents the application of better manuscript evidence. From the ordinary believer seat in a KJV-Only congregation, the doctrine operates as spiritual manipulation—they are told their difficulty understanding the text is spiritual failure, not a translation clarity issue, and this reframing serves the institutional interest in gatekeeping. The engine computes these divergent classifications from the structural data (different exit options, different power levels, different relationships to the beneficiary/victim structure). The authored claim (snare) reflects the analytical consensus outside the benefiting parties; the metrics (high extractiveness, high suppression, rising theater) are descriptively accurate to the doctrine's actual operation.
 *
 * DIRECTIONALITY LOGIC:
 *   The KJV-Only leadership holds directionality near the beneficiary end (d ≈ 0.15): they collect the extraction (authority, revenue, institutional standing), control the rules, and have arbitrage-grade exit (they could switch readings if institutional pressure mounted). Evangelical scholars sit near the target end (d ≈ 0.85): their work is suppressed, they face institutional exclusion, and their exit is constrained (they cannot practice their profession in KJV-Only institutions without renouncing their scholarship). Ordinary believers sit at the target end (d ≈ 0.95): they are trapped (exiting the congregation means losing community, family, spiritual identity), identity-locked (their faith is fused with the institutional framework), and bear the full cost of intellectual suppression. Modern translation publishers sit at moderate target (d ≈ 0.70): their markets are partially suppressed within KJV-Only institutions but they have mobile exit (they publish to the much larger evangelical mainstream). The structural beneficiary/victim declaration (beneficiaries: kjv_only_leadership, pulpit_gatekeepers; victims: modern_translation_publishers, evangelical_scholars, ordinary_believers_seeking_clarity) feeds these directionality values; the engine derives d automatically from power, exit, and these declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reliable English Bible in a world of modernist higher criticism) was live in the 1920s-1950s. It is now dead: evangelical scholarship has produced modern translations using earlier manuscripts, and these translations are widely trusted even in conservative institutions. The doctrine persists despite the founding problem being solved, which is the signature of mandatrophy—an institutional arrangement that has outlived its function and is now pure extraction dressed as coordination. The 'founding problem status = dead' declaration combined with 'disappearance verdict = world rearranges' triggers the mandatrophy flag in downstream analysis: the constraint no longer solves a real coordination problem (the Bible now has multiple reliable English versions) but continues to extract authority and revenue for institutional gatekeepers. The rising theater ratio (enforcement activity devoted to defending the gate rather than providing a good translation) confirms mandatrophy: the doctrine's operation is increasingly theatrical—sermons on the superiority of the KJV despite the manuscript evidence pointing the opposite direction, suppression of modern translations despite their wider adoption, institutional identity locked into a position the doctrine's own authority structure (evangelical theologians, textual scholars) has abandoned.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'What proportion of the measured suppression is structural (external barriers: institutional rules, publication bans, pulpit exclusion) vs. internalized (believers'' own mental frameworks: learned fear of alternatives, identity fusion with the doctrine)?',
    'Longitudinal study of believers who exit KJV-Only institutions: do they immediately adopt modern translations, or do they continue to experience suppression even after external barriers are removed? Post-exit behavior reveals the internalized component.',
    'If suppression is mostly structural, removing institutional barriers would allow rapid adoption of alternatives. If mostly internalized, the doctrine would persist through psychological mechanisms even after institutional gatekeeping weakened. This changes the classification of the exit mechanism: trapped (structural) vs. identity_locked (internalized).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Proportion and mechanism of suppression: external barriers vs. internalized belief.').

omega_variable(
    doctrine_vs_scholarship_boundary,
    'Is the KJV-Only doctrine a good-faith theological position grounded in genuine textual arguments, or is it fundamentally a power grab using theological framing?',
    'Content analysis of KJV-Only literature: Do the theological arguments engage scholarly evidence and respond to counterarguments (good faith), or do they dismiss contrary evidence categorically as satanic/unregenerate without substantive engagement (power grab)? Interviews with gatekeepers about their own doubts regarding manuscript evidence.',
    'If good faith, the constraint is a sincere disagreement about textual authority—possibly a rope or tangled rope with coordination and extraction mixed. If power grab, it is pure snare—the theological language is cover for institutional rent-seeking. This shifts the mandatrophy analysis: good-faith error vs. deliberate deception.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(doctrine_vs_scholarship_boundary, conceptual, 'Whether the doctrine is good-faith theology or institutional power grab.').

omega_variable(
    sibling_reading_displacement,
    'Do the functional-equivalence and revisable-translation readings represent genuine alternatives held by different parties within the evangelical movement, or are they being actively suppressed by the exclusive-inspiration reading''s gatekeeping apparatus?',
    'Institutional mapping: Which evangelical institutions endorse each reading? Do institutions that held revisable-translation or functional-equivalence readings 30 years ago still hold them, or have they shifted toward exclusive-inspiration? Survey data on reading prevalence across denominations.',
    'If the siblings are coexisting readings held by different parties (weak suppression), the constraint is more rope-like—different denominations have different standards, both valid within their contexts. If exclusive-inspiration is actively suppressing siblings within institutions that once held them (strong suppression), the constraint is more snare-like—it is a monopoly position defended against alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_displacement, empirical, 'Whether sibling readings coexist or are being actively suppressed by exclusive-inspiration gatekeeping.').

omega_variable(
    kernel_reading_distinction,
    'Are the three readings of the KJV text distinct constraints, or are they versions of a single constraint viewed from different seats?',
    'ε-invariance test: For each reading, measure extractiveness from the perspective of each reading''s core framing. If ε differs substantially across readings (exclusive-inspiration reading shows high extractiveness from evangelical scholar seat, but functional-equivalence reading shows low extractiveness from the same seat), then each reading is a distinct constraint with its own ε. If ε stays constant across readings (the measurement reflects an objective property of the text''s role in the world), then the readings are perspectives on one constraint.',
    'If distinct constraints, the three stories (exclusive-inspiration, functional-equivalence, revisable-translation) should be authored separately with different ε values, different victim sets, and linked via network.affects_constraints. If one constraint, the three readings are per-seat perspectives and should be authored as alternative stakeholder analyses within a single story. The corpus decomposition strategy depends on this resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Whether the three readings are distinct constraints or perspectives on one constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__exclusive_inspiration_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(kjv__tr_t5, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement(kjv__tr_t15, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(kjv__tr_t25, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(kjv__tr_t35, kjv_text_1611__exclusive_inspiration_reading, theater_ratio, 35, 0.42).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(kjv__be_t5, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 5, 0.66).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(kjv__be_t15, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(kjv__be_t25, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 25, 0.77).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(kjv__be_t35, kjv_text_1611__exclusive_inspiration_reading, base_extractiveness, 35, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(kjv__su_t0, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(kjv__su_t5, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 5, 0.71).
narrative_ontology:measurement(kjv__su_t10, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 10, 0.74).
narrative_ontology:measurement(kjv__su_t15, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 15, 0.76).
narrative_ontology:measurement(kjv__su_t20, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(kjv__su_t25, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(kjv__su_t30, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 30, 0.81).
narrative_ontology:measurement(kjv__su_t35, kjv_text_1611__exclusive_inspiration_reading, suppression_requirement, 35, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_text_1611__exclusive_inspiration_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(kjv_text_1611__exclusive_inspiration_reading, 0.12).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__functional_equivalence_reading).
narrative_ontology:affects_constraint(kjv_text_1611__exclusive_inspiration_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'KJV text of 1611'. The exclusive-inspiration reading treats the KJV as uniquely divinely authorized and all other translations as corrupted or inferior, enabling gatekeeping and extraction. The functional-equivalence reading treats multiple translations as serving complementary purposes with no single authoritative version. The revisable-translation reading treats the KJV as historically important but improvable through modern scholarship. Each reading has its own ε value (exclusive-inspiration is highly extractive via gatekeeping; functional-equivalence is low-extractiveness coordination; revisable-translation is low-extractiveness rope). The three stories are linked via network.affects_constraints because they compete for institutional adoption and scholarly legitimacy—the dominance of each reading shapes the others' operating environment. The exclusive-inspiration reading actively suppresses its siblings by categorizing alternative readings as spiritually dangerous.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
