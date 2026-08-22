% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Exclusive Covenant Through Isaac (Abrahamic Covenant, Isaac Reading)
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   Genesis 17:19-21 records God's covenant with Abraham, emphasizing the
 *   transmission through Isaac: 'My covenant I will establish with Isaac,
 *   whom Sarah will bear to you at this set time next year... But as for
 *   Ishmael, I have heard you; I will bless him, too... yet My covenant I
 *   will establish with Isaac' (Genesis 17:19-21, JPS translation). The
 *   Isaac-exclusive reading interprets this passage as dividing the covenant
 *   into two streams: an exclusive covenant of chosenness through Isaac
 *   (transmitted to Jacob/Israel), and a subordinate blessing of material
 *   prosperity for Ishmael and his descendants, but not covenant inclusion.
 *   This reading has become institutionalized in Jewish theological tradition
 *   as foundational to Jewish chosenness doctrine and the exclusive
 *   legitimacy of Jewish covenantal standing. It is also defended in
 *   Christian supersessionist contexts as validation of Christian replacement
 *   of Israel. But it is contested: an alternative inclusive reading (the
 *   Ishmael reading) argues the text's covenant language encompasses Ishmael
 *   as well, and Islamic tradition reads the covenant as continuing through
 *   Ishmael to Muhammad. The constraint story narrates the Isaac-exclusive
 *   reading as a structural arrangement that benefits institutional Jewish
 *   continuity and exclusively-covenantal doctrines, while extracting
 *   recognition and covenantal standing from Ishmaelite claimants and Islamic
 *   tradition. The constraint's persistence depends on active enforcement of
 *   this reading's authority in Jewish textual interpretation and
 *   institutional religious authority. The measured extraction (0.82
 *   terminal) is high because the constraint excludes an entire lineage from
 *   covenantal standing while claiming to implement a divine mandate;
 *   suppression is substantial (0.71) because alternative readings must be
 *   actively kept out of legitimate discourse; theater is moderate (0.28)
 *   because much of the reading's enforcement is textual/interpretive
 *   (argument-based), not purely institutional command.
 *
 * KEY AGENTS:
 *   - jewish_institutional_authority: rabbinical councils, denominational hierarchies, theological academies that maintain and transmit the Isaac-exclusive reading
 *   - jewish_faithful_communities: believers whose identity and theological legitimacy rest on the chosenness doctrine enabled by the exclusive reading
 *   - ishmaelite_descendant_claimants: Arab, Palestinian, and other communities claiming Abrahamic covenant standing through Ishmael
 *   - islamic_theological_tradition: scholars and authorities reading the covenant as inclusive of Ishmael and extending through Islamic prophetic succession
 *   - christian_supersessionist_institutions: Christian theological bodies that use the Isaac reading to argue for Christian replacement of Israel
 *   - contemporary_comparative_theology: academic and interfaith scholars proposing alternative readings emphasizing covenant inclusivity
 *   - textual_scholars: biblical exegetes and historians examining the original meaning of Genesis 17:19-21
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.82).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.71).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Exclusive Covenant Through Isaac (Abrahamic Covenant, Isaac Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '0b9402f3-a10b-432b-b700-162d10b3a1c9').
narrative_ontology:cs_kernel_codification('0b9402f3-a10b-432b-b700-162d10b3a1c9', fixed_text).
narrative_ontology:cs_authority_grounding('0b9402f3-a10b-432b-b700-162d10b3a1c9', lineage).
narrative_ontology:cs_interpretation_layer_present('0b9402f3-a10b-432b-b700-162d10b3a1c9').
narrative_ontology:cs_reading_relation('0b9402f3-a10b-432b-b700-162d10b3a1c9', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('0b9402f3-a10b-432b-b700-162d10b3a1c9', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_axiom('0b9402f3-a10b-432b-b700-162d10b3a1c9', foundational, covenant_transmission_lineally_exclusive).
narrative_ontology:cs_axiom_status(covenant_transmission_lineally_exclusive, holdable).
narrative_ontology:cs_axiom_grounding('0b9402f3-a10b-432b-b700-162d10b3a1c9', covenant_transmission_lineally_exclusive, deontological).
narrative_ontology:cs_axiom('0b9402f3-a10b-432b-b700-162d10b3a1c9', secondary, jewish_chosenness_doctrine_enabled).
narrative_ontology:cs_axiom_status(jewish_chosenness_doctrine_enabled, holdable).
narrative_ontology:cs_axiom_grounding('0b9402f3-a10b-432b-b700-162d10b3a1c9', jewish_chosenness_doctrine_enabled, deontological).
narrative_ontology:cs_reference_frame('0b9402f3-a10b-432b-b700-162d10b3a1c9', exclusive_isaac_covenant).
narrative_ontology:cs_drift_state('0b9402f3-a10b-432b-b700-162d10b3a1c9', contemporary_pluralist_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b9402f3-a10b-432b-b700-162d10b3a1c9', '2026-06-12T14:23:47Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_continuity).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_descendant_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_faith_communities).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_institutions).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_theological_tradition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Rabbinical councils, theological academies, denominational hierarchies (Orthodox, Conservative, Reform, and Jewish renewal movements) maintain, teach, and defend the Isaac-exclusive reading as foundational to Jewish theology and identity. They interpret Genesis 17:19-21 and related texts, establish canon law (Halakha) based on this interpretation, and control institutional education and authority. They set the terms of who participates in covenant and who does not. Their institutional standing and authority rest on the legitimacy of the reading they maintain.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_authority, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Jewish believers whose identity, belonging, and theological framework depend on the chosenness doctrine enabled by the exclusive covenant reading. They receive the benefit of a coherent identity, institutional community, and theological legitimacy. Their exit from this identity is culturally and religiously impossible — identity_locked because the constraint is constitutive of their identity itself.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_faith_communities, beneficiary,
    moderate, civilizational, identity_locked, global).

% Arab, Palestinian, and other communities tracing descent from Ishmael who claim Abrahamic covenant standing and inheritance. They bear the cost of systematic exclusion from covenantal legitimacy based on the Isaac reading. Their claims to covenant-grounded identity and rights are delegitimized by the reading. They are trapped in the constraint because their very identity (as descendants of Ishmael) is what the constraint excludes — they cannot exit without abandoning their genealogical and identity claims.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_descendant_claimants, payer,
    moderate, civilizational, identity_locked, global).

% Islamic scholars, theologians, and religious authorities who read the Abrahamic covenant as inclusive of Ishmael and extending through Islamic prophetic succession (Ishmael → Muhammad). They bear the cost of having their reading systematically delegitimized and excluded from mainstream Abrahamic discourse by the Isaac reading. Islamic tradition's claim to Abrahamic legitimacy and prophetic authority is undermined. They are constrained in exit because rejecting the Abrahamic covenant entirely would abandon a core legitimacy source for Islamic theology, but accepting the Isaac reading means abandoning their own covenantal claims.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_theological_tradition, payer,
    institutional, civilizational, constrained, global).

% Christian theological bodies (Catholic, Orthodox, mainstream Protestant) that use the Isaac reading to argue for Christian replacement of Israel in covenantal standing. They benefit from the reading insofar as it validates their claim that Jews lost covenantal status (which Christians then inherit). However, they are also observers: the constraint's primary operation is Jewish institutional continuity, not Christian beneficiary. Their exit from supersessionism would require reframing entire Christological theology, making them constrained rather than freely mobile.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_institutions, beneficiary,
    institutional, civilizational, constrained, global).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_institutions, observer).

% Academic scholars, interfaith activists, and theological innovators proposing alternative, inclusive readings of the Abrahamic covenant that would transcend the Isaac/Ishmael boundary. They are excluded from institutional Jewish theological authority because they question the foundational reading. They have high exit mobility (they can pursue careers outside religious institutional structures) but are systematically kept out of authoritative theological discourse within institutional Judaism.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, contemporary_comparative_theology, excluded,
    moderate, biographical, mobile, global).

% Academic historians, philologists, and exegetes who examine Genesis 17:19-21 in its original context and compare interpretations across traditions. They can provide evidence about whether the text explicitly forecloses Ishmael or permits inclusive readings. Their findings can support or challenge the reading's textual legitimacy. They are observers with professional mobility — they can pursue careers in secular academia independent of theological institutional control.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, biblical_textual_scholars, observer,
    moderate, biographical, mobile, global).

% The text of Genesis 17:19-21 itself, treated as an entity for analytical purposes. It is the fixed reference point that all readings interpret. As a non-agent, it is excluded from beneficiary/victim derivation but is included for narrative completeness — all readings in the Abrahamic covenant family rest on this text.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, genesis_kernel, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(abrahamic_covenant__isaac_covenant_reading, genesis_kernel).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_authority).
narrative_ontology:fixing_cost_class(abrahamic_covenant__isaac_covenant_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains Jewish covenantal identity and institutional continuity: the exclusive Isaac reading provides a theological foundation for Jewish distinctiveness, chosenness, and institutional authority to interpret and transmit covenant. It solves the coordination problem of how Israel maintains coherent identity and religious legitimacy across time and dispersion.
% TRANSFER_FUNCTION: Transfers covenantal standing and theological legitimacy FROM Ishmaelite claimants and Islamic tradition TO Jewish institutional authority and Jewish faith communities. It moves recognition, authority to interpret covenant, and the claim to be the 'chosen people' exclusively to the Jewish institutional seat. It also transfers legitimacy FROM alternative (inclusive) readings TO the exclusive reading through institutional gatekeeping and textual authority control.
% ABSENT_VOICES: Ishmaelite/Islamic claimants and contemporary comparative theologians are largely excluded from authoritative theological institutions that define the reading. Ishmaelite claimants would argue that Genesis 17 includes Ishmael and that the text's emphasis on Isaac does not mandate exclusion; Islamic theology would argue that the covenant continues through Ishmael to Muhammad and that Islamic tradition has full Abrahamic standing. Comparative theologians would argue for readings that transcend the exclusive boundary and recognize Abrahamic covenant as shared across religions. These voices are structurally kept out of institutional Jewish theological discourse — they have platforms in academic theology, Islamic tradition, and interfaith movements, but not in authoritative Jewish institutional interpretation.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared overnight — if the Isaac-exclusive reading lost institutional authority and alternative inclusive readings were admitted to legitimate theological discourse — the religious and political landscape would reorganize substantially. Jewish institutional identity would need to reframe chosenness doctrine (possibly as moral responsibility rather than exclusive covenantal standing); Islamic tradition and Ishmaelite claimants would gain theological legitimacy for their Abrahamic inheritance claims; geopolitical claims grounded in covenant (particularly Israeli territorial claims derived from covenantal land promises) would lose a primary legitimacy source; interfaith relations would reframe toward more symmetric Abrahamic partnership; Jewish-Muslim conflict narratives would lose scriptural-exclusive grounding. The constraint's disappearance would reshape institutional religious authority, theological education, and geopolitical legitimacy claims.
% FOUNDING_PROBLEM: How does Israel maintain its covenant standing with God and distinguish itself as the chosen people among other religions?
% FOUNDING_PROBLEM_CORROBORATION: Jewish institutional theology and rabbinical tradition attest the founding problem is live: Jewish chosenness and covenant standing remain central to Jewish identity and institutional theology (Orthodox, Conservative movements especially). However, outside corroboration from comparative religion scholars and Islamic/Ishmaelite claimants contests this: they argue the founding problem has been reframed by secular pluralism and modern interfaith dialogue — the question now is not 'how does Israel maintain exclusive covenant standing' but 'how do we recognize shared Abrahamic inheritance across religions.' Contemporary Jewish renewal and Reform movements also produce internal debate on whether chosenness doctrine is sustainable in pluralist contexts. The founding problem has shifted from theology to geopolitics: it now primarily grounds territorial claims (Israeli state legitimacy via covenant land promises) rather than pure theological distinctiveness, as secular nationalism has partially replaced theology as identity ground.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (terminal 0.82) because the constraint performs two functions simultaneously: (1) it establishes an identity boundary that excludes Ishmael and his lineage from covenantal standing, and (2) it grounds institutional Jewish claims to exclusive covenantal legitimacy that can be deployed to support material claims (territorial, political, legal). The extraction is not merely theological — it is a reading that powers actual exclusions. The measurement series from t=0 (earliest rabbinic formalization) to t=2000 (contemporary era) shows rising extractiveness as the reading became increasingly institutional: early phases (t=0-400) involved interpretive competition and theological debate (lower extraction); middle phases (t=400-1200) involved crystallization into institutional authority and formal rejection of alternatives (rising extraction); late phases (t=1200-2000) show high extraction maintained through institutional gatekeeping and textual authority, with theater_ratio rising as explicitly covenantal exclusion rhetoric becomes softened by interfaith language and theological reframing. Suppression rises substantially over the interval as alternative readings emerge and must be actively suppressed: early suppression is low because alternatives have not yet formed; later suppression is high because textual scholars, Islamic tradition, and interfaith movements all propose alternative readings that must be excluded from legitimate institutional discourse. Theater ratio is lowest in early phases (the reading is straightforward textual claim) and rises in late phases as institutional defense of the reading becomes more rhetorical and less textual-literal. The claimed type is tangled_rope: the constraint both coordinates Jewish institutional identity and communal belonging (rope function) while simultaneously extracting covenantal standing from Ishmael and his descendants (extraction function). The extraction is asymmetric — institutional Jewish communities benefit while Ishmaelite claimants and Islamic tradition bear costs — and the arrangement requires active enforcement to persist (textual gatekeeping, institutional control of biblical interpretation, suppression of alternative readings). The claim/metric gap is deliberate: the constraint is claimed as tangled_rope (mixed coordination/extraction) while the authored metrics show high extraction, high suppression, and rising theater — the engine measures how the reading actually operates in institutional context, not what its theological framing claims.
 *
 * PERSPECTIVAL GAP:
 *   The Jewish institutional beneficiary seat (rabbinical authority, community leaders) experiences the constraint as coordination: it establishes and maintains Jewish identity, community continuity, and the theological foundation for Jewish chosenness and standing. From this seat, the reading is a coordination function solving the problem of how Israel maintains its covenantal status and identity. The Ishmaelite/Islamic claimant seat experiences the same constraint as pure extraction: a reading that systematically denies them standing, legitimacy, and the ability to claim Abrahamic covenant inheritance. From this seat, the constraint is enforced exclusion grounded in a contested textual reading. A hypothetical Christian supersessionist seat experiences a mixed function: they benefit from the reading insofar as it validates Christian displacement of Jewish covenant (extraction from Jews, subsidy to Christians), but they also use the reading as theology not identity-foundation (the coordination benefit is to Christian institutions, not their own exclusive standing). The engine computes per-seat directionality from the structural data: Jewish institutional beneficiaries get d near the beneficiary end (low extraction for them, positive subsidy flow); Ishmaelite/Islamic claimants get d near the target end (high extraction, loss of standing); Christian institutions get a mixed d (they receive theological ammunition but are not the primary beneficiaries of Jewish covenantal exclusivity). The perspectival gap is structural: the same reading operates as beneficial coordination for one seat and extractive exclusion for another.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish institutional continuity (beneficiary): The constraint enables and sustains Jewish covenantal standing, identity, and institutional authority. Rabbinical institutions benefit directly from the exclusive reading because it grounds their authority to interpret and transmit covenant. From the beneficiary seat, the constraint solves a genuine problem (how does Israel maintain its covenantal standing and distinguish itself from other Abrahamic religions) and persists because it is institutionally maintained and theologically defended. Directionality for this agent: d~0.1 (near full beneficiary), low effective extraction. Ishmaelite descendant claimants (victims): The constraint denies them covenantal standing, legitimacy as participants in Abrahamic covenant, and grounds for claims based on covenant inheritance. This is a structured exclusion that requires suppression to maintain (alternative readings and Ishmaelite/Islamic claims must be actively rejected). Directionality for this agent: d~0.92 (near full target), high effective extraction. Islamic theological tradition (victims): Similar to Ishmaelite claimants, but institutional: Islamic theology claims the covenant continues through Ishmael to Muhammad, and the Isaac-exclusive reading delegitimizes this claim. The constraint extracts Islamic tradition's ability to claim Abrahamic legitimacy and grounds for theological authority. Directionality: d~0.89 (near full target), high effective extraction. Christian supersessionist institutions (complex mixed): These benefit from the reading insofar as it validates Christian displacement of Jewish covenant (d~0.25, moderate beneficiary), but they are not the primary beneficiaries — the reading's main effect is to benefit Jewish institutional authority, not Christian. The extraction is deflected: Christians use the Jewish reading to justify their own displacement theology, but they do not directly extract from the constraint's operation. Exit options for all seats are deeply constrained: Jewish institutions cannot abandon the reading without losing covenantal legitimacy; Islamic and Ishmaelite actors cannot exit the constraint without abandoning their own covenantal claims; Christian institutions are identity_locked into supersessionist theology and cannot easily adopt inclusive readings without theological reorganization.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (t=0): How does Israel maintain its covenant standing and distinguish itself as the chosen people? This is a live problem for Jewish institutional identity and theology. The founding problem persists into the contemporary era: Jewish chosenness doctrine remains central to Jewish identity, and the exclusive covenant reading continues to be invoked to ground Jewish institutional claims and theological distinctiveness. The constraint does NOT exhibit classic mandatrophy (founding problem solved but constraint persists). Instead, it exhibits what might be called 'identity mandatrophy': the founding problem (covenant distinctiveness) is structurally permanent — it is reframed rather than solved. As secularization and pluralism advance, the founding problem mutates: Jews no longer invoke the covenant primarily for theological distinctiveness (Christianity and Islam now share Abrahamic identity broadly), but the constraint persists because it grounds institutional Jewish claims to land, law, and political standing (via Israel-state theology). The extractive function increases relative to the coordinative function over time: early phases involve genuine theological coordination (how do we establish chosenness?), late phases involve instrumental extraction (how do we ground territorial and political claims?). The measured rising extraction reflects this mutation. The constraint does NOT disappear because it is now deployed for institutional and state legitimacy, not merely theological coordination. A mandatrophy flag would trigger if theater_ratio rose above ~0.65 (indicating mostly performative maintenance) and disappearance_verdict were 'world_unchanged' (founding problem solved but constraint persists theatrically); current metrics show theater_ratio only at 0.28 and the founding problem remains institutionally live, so classic mandatrophy is not triggered. However, the rising extraction and theater over time, combined with the constraint's role in grounding geopolitical claims, suggest the constraint is shifting from theological coordination toward political/territorial extraction — a drift that warrants monitoring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_identity,
    'Is this a reading of a contested kernel (the Abrahamic covenant) or a standalone theological claim?',
    'Genealogical analysis: the reading''s legitimacy depends on grounding in Genesis 17:19-21 as its authority source. If the text-kernel is abandoned or reinterpreted, the reading''s standing shifts. Trace which institutional actors invoke Genesis 17 as binding authority.',
    'If kernel-grounded: the constraint''s persistence depends on maintaining the authority of the founding text and the reading''s interpretation of it. If the reading is reframed as post-hoc theological doctrine rather than textual reading, its ε and enforcement mechanism change fundamentally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether this constraint is a kernel reading or a derived theological doctrine.').

omega_variable(
    exclusion_mechanism_structural_vs_interpretive,
    'Is the exclusion of Ishmael a direct textual mandate or an interpretive choice reading restrictive language into permissive text?',
    'Comparative reading analysis: examine whether Genesis 17:19-21 explicitly forecloses Ishmael or merely directs emphasis to Isaac. Cross-reference with Jewish, Christian, and Islamic exegetical traditions. Documentary evidence from early rabbinic commentary (Talmud, Midrash) on whether the text permits or mandates Ishmaelite exclusion.',
    'If the mandate is direct (text-explicit): the constraint is a literal covenant constraint with high accessibility collapse (alternatives are logically unavailable). If interpretive: the constraint''s suppression is higher (the exclusion must be enforced through selective reading and institutional control of textual authority), and alternative readings become empirically live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_structural_vs_interpretive, empirical, 'Whether Ishmael''s exclusion is textually explicit or interpretively enforced.').

omega_variable(
    institutional_beneficiary_identity,
    'Does the constraint benefit ''Jewish continuity'' as an institutional entity, individual Jewish believers, or the covenant''s theological legitimacy as such?',
    'Institutional analysis: identify which organizational actors (Jewish denominations, state entities, diaspora communities) actively defend the Isaac-exclusive reading and what material benefits accrue to them (territorial claims, legal status, diaspora identity maintenance). Distinguish between theological benefit (the reading vindicates doctrine) and material benefit (the reading powers institutional claims).',
    'If the primary beneficiary is institutional Jewish authority structures: the constraint is extractive beyond its coordination function — the exclusive reading powers material exclusions (territorial/legal claims). If the primary beneficiary is theological coherence, the extraction metric is lower and the constraint becomes more clearly rope-like. The beneficiary''s identity shifts the constraint''s classification between the payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_beneficiary_identity, empirical, 'Whether the beneficiary is institutional, theological, or both.').

omega_variable(
    alternative_reading_suppression,
    'In contemporary Jewish, Christian, and Islamic communities, how actively are alternative readings (inclusive Abrahamic covenant, Ishmaelite covenant validity) suppressed, and through what mechanisms?',
    'Ethnographic and textual analysis of contemporary religious communities: examine which readings are taught in seminaries and schools, which are published in official commentary, which are marginalized as heterodox. Survey institutional religious authority (rabbinical councils, theological academies, fatwa-issuing bodies) on whether they permit, tolerate, or actively suppress alternative readings.',
    'High suppression (active enforcement, alternatives excluded from legitimate discourse) supports the tangled_rope classification and high measured suppression. Low suppression (alternatives coexist as live theological positions) would shift the classification toward rope and lower suppression values. The measured suppression metric is empirically tied to the enforcement machinery''s operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_suppression, empirical, 'The degree to which alternative readings are institutionally suppressed.').

omega_variable(
    victim_set_scope_and_standing,
    'Who are the actual victims of this constraint''s operation: Ishmaelite historical descendants (now Arabs), Islamic theological tradition, or abstract covenantal claimants?',
    'Institutional and genealogical analysis: trace who invokes covenant exclusion as grounds for material claims (land, recognition, legitimacy) and who bears costs from that exclusion. Examine whether Ishmaelite/Arab/Islamic actors accept the constraint''s frame (acknowledge exclusion) or reject it (contest the reading). Document whether the exclusion''s enforcement is active (legal, territorial, diplomatic) or passive (institutional dismissal).',
    'If victims are primarily institutional (Islamic tradition as a religious system competing for legitimate Abrahamic inheritance): the constraint is a theological/identity boundary with moderate direct enforcement costs. If victims are material claimants (populations dispossessed via covenant-grounded territorial claims): the constraint''s extraction includes material dispossession and suppression is higher. The victim identity shifts the directionality from payer seats and the constraint''s effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_scope_and_standing, empirical, 'The identity and location of the constraint''s victims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(abra_tr_t400, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 400, 0.12).
narrative_ontology:measurement(abra_tr_t800, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 800, 0.18).
narrative_ontology:measurement(abra_tr_t1200, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1200, 0.24).
narrative_ontology:measurement(abra_tr_t1600, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1600, 0.27).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2000, 0.28).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(abra_be_t400, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 400, 0.62).
narrative_ontology:measurement(abra_be_t800, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 800, 0.71).
narrative_ontology:measurement(abra_be_t1200, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1200, 0.76).
narrative_ontology:measurement(abra_be_t1600, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1600, 0.8).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2000, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(abra_su_t400, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 400, 0.48).
narrative_ontology:measurement(abra_su_t800, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 800, 0.58).
narrative_ontology:measurement(abra_su_t1200, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1200, 0.65).
narrative_ontology:measurement(abra_su_t1600, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1600, 0.69).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2000, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__land_promise_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, jewish_chosenness_doctrine__institutional_authority).

% DUAL FORMULATION NOTE:
% This constraint is part of the Abrahamic covenant family (kernel_id: abrahamic_covenant). The Isaac-exclusive reading is decomposed from the ishmael-inclusive reading (abrahamic_covenant__ishmael_covenant_reading) because they produce different ε values (0.82 vs. 0.35) and different victim sets from the same kernel, violating ε-invariance if folded into one story. The land_promise_constraint (territorial dimension of covenant) is a separate decomposition because covenant_identity and land_allocation have different beneficiary structures (institutional Jewish continuity vs. territorial Israeli claims) and different sibling readings. All three stories share the kernel_id and are linked via network.affects_constraints so the constraint family is visibly connected. The relationship is: isaac_covenant_reading (this story) affects ishmael_covenant_reading (if the Isaac reading is weakened, Islamic readings become live) and affects land_promise_constraint (covenant identity feeds into land legitimacy claims).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
