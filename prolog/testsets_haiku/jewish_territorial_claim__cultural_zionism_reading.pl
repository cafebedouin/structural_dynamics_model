% ============================================================================
% CONSTRAINT STORY: jewish_territorial_claim__cultural_zionism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_territorial_claim__cultural_zionism_reading, []).

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
 *   constraint_id: jewish_territorial_claim__cultural_zionism_reading
 *   human_readable: Jewish Cultural-Spiritual Center in Palestine (Cultural Zionism Reading)
 *   domain: political_history/nationalism/settler_colonialism
 *
 * SUMMARY:
 *   This constraint story instantiates the cultural Zionism READING of the
 *   Jewish territorial claim kernel. The reading proposes a Jewish cultural
 *   and spiritual center in Palestine, emphasizing intellectual renaissance,
 *   Hebrew language revival, artistic flourishing, and agricultural
 *   innovation — WITHOUT requiring political sovereignty or demographic
 *   Jewish majority. The reading envisions potential binational coexistence
 *   where Arabs and Jews share institutions and governance. This is ONE
 *   constraint among four sibling readings: political Zionism (Jewish state
 *   as answer to antisemitism), labor Zionism (national regeneration through
 *   socialist settlement and 'conquest of labor'), and revisionist Zionism
 *   (maximalist territorial claims enforced by military power). The
 *   constraint is authored INDEPENDENTLY: its extractiveness, suppression,
 *   and beneficiary structure reflect what the cultural Zionism reading
 *   itself claims, not a composite or median of all readings. The engine will
 *   compute how each seat experiences this constraint; divergence between the
 *   reading's own claim and the per-seat computed type reveals structural
 *   tensions the reading cannot contain.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_seekers: global diaspora Jews motivated by spiritual rootedness and cultural renewal; moderate power; retain high exit options (can choose not to migrate, or migrate elsewhere). Structurally beneficiary under the reading.
 *   - palestinian_arab_residents: Arab population already in Palestine who would coexist under binational framework; powerless structural position; constrained exit (property and communities tied to land). Theoretically beneficiary from cultural exchange and economic development but face inevitable demographic pressure and minority status.
 *   - hebrew_cultural_movement: intellectuals and artists (Ahad Ha'am lineage) setting the reading's priorities; organized power; frame the project as cultural-intellectual rather than political-territorial. Agenda-setters for what gets built and how it is justified.
 *   - ottoman/mandate_administrative_authority: colonial power structure (Ottoman until 1918, then British Mandate) that formally authorizes settlement; institutional power; maintains sovereignty floor beneath the cultural autonomy the reading envisions. Constrains what is possible without enforcing it.
 *   - arab_nationalist_movements: pan-Arab and Palestinian nationalist factions (post-WWI emergence) who reject the reading's premises and view settlement as colonial occupation regardless of cultural framing. Structurally excluded from the reading's conversation despite being directly affected and actively resisting.
 *   - political_zionist_factions: Herzl/Jabotinsky streams demanding state sovereignty and demographic majority; excluded from this reading's deliberation; represent an alternative framework the reading's axioms foreclose or marginalize.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_territorial_claim__cultural_zionism_reading, 0.42).
domain_priors:suppression_score(jewish_territorial_claim__cultural_zionism_reading, 0.35).
domain_priors:theater_ratio(jewish_territorial_claim__cultural_zionism_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jewish_territorial_claim__cultural_zionism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_territorial_claim__cultural_zionism_reading, rope).
narrative_ontology:human_readable(jewish_territorial_claim__cultural_zionism_reading, "Jewish Cultural-Spiritual Center in Palestine (Cultural Zionism Reading)").
narrative_ontology:topic_domain(jewish_territorial_claim__cultural_zionism_reading, "political_history/nationalism/settler_colonialism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_territorial_claim__cultural_zionism_reading, 'c8ebd718-11f9-475c-9b9a-43677d679fd3').
narrative_ontology:cs_kernel_codification('c8ebd718-11f9-475c-9b9a-43677d679fd3', distributed).
narrative_ontology:cs_authority_grounding('c8ebd718-11f9-475c-9b9a-43677d679fd3', distributed).
narrative_ontology:cs_reading_relation('c8ebd718-11f9-475c-9b9a-43677d679fd3', jewish_territorial_claim__political_zionism_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8ebd718-11f9-475c-9b9a-43677d679fd3', jewish_territorial_claim__labor_zionism_reading, influences).
narrative_ontology:cs_reading_relation('c8ebd718-11f9-475c-9b9a-43677d679fd3', jewish_territorial_claim__revisionist_zionism_reading, coexists_with).
narrative_ontology:cs_axiom('c8ebd718-11f9-475c-9b9a-43677d679fd3', foundational, cultural_autonomy_without_political_sovereignty).
narrative_ontology:cs_axiom_status(cultural_autonomy_without_political_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('c8ebd718-11f9-475c-9b9a-43677d679fd3', cultural_autonomy_without_political_sovereignty, deontological).
narrative_ontology:cs_axiom('c8ebd718-11f9-475c-9b9a-43677d679fd3', foundational, binational_coexistence_feasible).
narrative_ontology:cs_axiom_status(binational_coexistence_feasible, holdable).
narrative_ontology:cs_axiom_grounding('c8ebd718-11f9-475c-9b9a-43677d679fd3', binational_coexistence_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('c8ebd718-11f9-475c-9b9a-43677d679fd3', jewish_cultural_renaissance_in_palestine).
narrative_ontology:cs_drift_state('c8ebd718-11f9-475c-9b9a-43677d679fd3', post_1935_territorial_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8ebd718-11f9-475c-9b9a-43677d679fd3', '').
narrative_ontology:cs_kernel_id(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_seekers).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_minority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_territorial_claim__cultural_zionism_reading, arab_landowners).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_minority).
narrative_ontology:constraint_victim(jewish_territorial_claim__cultural_zionism_reading, arab_landowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Diaspora Jews seeking cultural renewal and spiritual rootedness in historical/ancestral land. The constraint promises a cultural and intellectual center where Hebrew language, Jewish traditions, and creative life can flourish without requiring political domination. They migrate by choice, retain exit options, and envision themselves as contributing cultural participants rather than enforcing political rule.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, jewish_diaspora_seekers, beneficiary,
    moderate, generational, mobile, global).

% Arab residents of Palestine who would coexist in a binational cultural center. Under the reading's logic, they would retain political representation and cultural autonomy; the Jewish presence would be cultural and intellectual rather than political-majoritarian. They benefit from cultural exchange and economic development but face structural inequality in a minority position and constrained choice about Jewish settlement density.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_minority, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, palestinian_arab_minority, payer).

% Intellectuals, artists, and educators (Ahad Ha'am, early Zionist cultural figures) who frame the project as spiritual and intellectual regeneration. They set priorities for what gets built: schools, theaters, publishing houses, agricultural communes oriented toward cultural self-expression rather than political conquest. They retain substantial discretion over the character of settlement.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_movement, agenda_setter,
    organized, generational, mobile, regional).

% Ottoman (or later British Mandate) administrative structure that authorizes land purchase and settlement. Under the reading, the constraint operates WITH rather than against existing administration; settlement is legal and regulated. The authority maintains formal control and receives tax revenue while cultural autonomy operates below the sovereignty ceiling.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, ottoman_palestinian_administrative_authority, agenda_setter,
    institutional, biographical, trapped, regional).

% Palestinian Arabs who sell or lease land to Jewish settlers. They benefit from purchase prices and rents but lose ancestral property and face social pressure from communities who view any land sale as national betrayal. Their choice is formally free but structurally constrained by economic need and community norms.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, arab_landowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(jewish_territorial_claim__cultural_zionism_reading, arab_landowners, payer).

% Political Zionists (Herzl, Jabotinsky streams) seeking Jewish state sovereignty and demographic majority who view cultural Zionism as insufficient. They would demand different settlement patterns, political structures, and military force. They are not in the conversation under this reading but represent a suppressed alternative framework.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, political_zionist_faction, excluded,
    organized, generational, constrained, regional).

% Arab nationalists (emerging post-WWI) who view any Jewish settlement, regardless of cultural framing, as colonial encroachment and demographic displacement. They would reject binational premises and demand exclusive Arab sovereignty. Their presence and objections are structural realities the constraint cannot actually silence despite the reading's inclusionary rhetoric.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, pan_arab_national_movement, excluded,
    organized, generational, constrained, regional).

% Historical, sociological, or political analysis seated outside the immediate contest, examining whether the cultural Zionism reading genuinely describes what settlement produced or served as cover narrative for political-territorial expansion.
narrative_ontology:constraint_stakeholder(jewish_territorial_claim__cultural_zionism_reading, academic_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_territorial_claim__cultural_zionism_reading, hebrew_cultural_movement).
narrative_ontology:fixing_cost_class(jewish_territorial_claim__cultural_zionism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a shared intellectual and cultural space where Jews and Arabs collaborate on language revival, artistic production, agricultural innovation, and educational institutions. Solves the diaspora's rootlessness through cultural reconnection while enriching Palestinian Arab society through technological and artistic exchange.
% TRANSFER_FUNCTION: Moves Jewish diaspora capital (financial, intellectual, educational) into Palestine to establish cultural institutions; moves land from Arab ownership into Jewish settlement; distributes cultural products (Hebrew literature, theater, music) to global Jewish and Arab audiences; establishes new social norms of coexistence and binational cultural life.
% ABSENT_VOICES: Political Zionists who would reject cultural-only framing as insufficient; Arab nationalists who view any Jewish settlement as colonial occupation regardless of cultural rhetoric; poorer Palestinians displaced by land purchases; non-Hebrew-speaking Jews skeptical of cultural revival as national project.
% DISAPPEARANCE_RATIONALE: From the reading's endorsing seat: if this constraint (the vision of a cultural center without political domination) disappeared, the opportunity for peaceful binational coexistence would evaporate and be replaced by zero-sum territorial competition. From the excluded nationalist seats: the constraint never actually existed as described — political expansion and demographic dominance were always the real goal, and 'cultural Zionism' was rhetorical cover; disappearing the rhetorical framing would clarify rather than reorganize anything.
% FOUNDING_PROBLEM: Jewish diaspora dispersion, cultural assimilation, and loss of Hebrew language and autonomous Jewish intellectual life created a spiritual crisis for Jewish identity. Antisemitism and marginalization made normal European existence precarious for Jewish communities. A cultural center in the ancestral land would renew Jewish civilization and prove Jewish capacity for national cultural regeneration without requiring political domination.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of cultural Zionism (Ahad Ha'am, early Zionist cultural figures, some contemporary scholars of Hebrew literature) attest the spiritual crisis was real and the cultural renewal was genuine. Critics (Arab historians, postcolonial scholars, revisionists within Jewish historiography) contest whether the founding problem was accurately stated or whether 'cultural renewal' was a rational-seeming framing for territorial ambitions that were political from the start. No uncontested external corroboration exists; the problem statement itself is the reading's contested axiom.
narrative_ontology:disappearance_verdict(jewish_territorial_claim__cultural_zionism_reading, contested).
narrative_ontology:founding_problem_status(jewish_territorial_claim__cultural_zionism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_territorial_claim__cultural_zionism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_territorial_claim__cultural_zionism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_territorial_claim__cultural_zionism_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_territorial_claim__cultural_zionism_reading_tests).
:- end_tests(jewish_territorial_claim__cultural_zionism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42 at interval end) is MODERATE because the reading genuinely offers Arabs cultural exchange, economic participation, and formal coexistence rights — it is not pure resource extraction. However, it DOES extract land (Arab properties purchased or settled), imposes Jewish cultural dominance in intellectual/institutional life, and structures a hierarchical coexistence where Arabs are minority partners in a Jewish-led project. Suppression (0.35) is relatively low because the reading does NOT frame itself as coercive; it imagines willing participation and legal settlement within Ottoman/Mandate frameworks. However, suppression RISES over the interval (0.15→0.35) as Arab nationalist resistance hardens and the constraint requires increasing enforcement against excluded Arab voices. Theater (0.22 at end, rising from 0.08) reflects a DISJUNCTION: the reading's own sincere cultural achievements (Hebrew literature, educational institutions, agricultural communes) are REAL, but they increasingly serve as rhetorical cover for territorial expansion that the reading itself does not acknowledge. By 1935-1945, the measured theater ratio shows performative emphasis on cultural accomplishments while settlement expansion and demographic dominance accelerate beneath the cultural narrative. The measurement series (1880-1948 at ~15-year intervals, 6 points per metric on one aligned grid) shows the reading's internal contradiction: genuine cultural productivity coupled with rising suppression and theater as political realities diverge from the reading's own avowed premises.
 *
 * PERSPECTIVAL GAP:
 *   CRITICAL DIVERGENCE: From the reading's ENDORSING seat (hebrew_cultural_movement, diaspora seekers), this constraint is a ROPE — genuine coordination solving a real spiritual problem, with Arabs as benefiting partners in binational culture. From the EXCLUDED Arab nationalist seat, this same structure is a SNARE — Jewish settlement framed as 'cultural' is in service of territorial consolidation and eventual political domination; the binational promises are rhetorical cover for demographic replacement. From the POWERLESS Palestinian resident seat, the structure oscillates: real cultural benefits and job creation mix with land loss, minority status, and inability to refuse Jewish settlement, producing a hybrid TANGLED ROPE from their perspective. The engine computes these divergences from power/exit/beneficiary data; the authored claim (rope) does not pre-adjudicate them. This perspectival gap is the reading's core structural ambiguity — whether cultural Zionism was a genuine alternative to political Zionism or its inevitable precursor.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows FROM the reading's own beneficiary/victim declarations AND structural power asymmetries. Diaspora seekers (moderate power, mobile exit) approach d=0.2-0.3 (beneficiaries subsidized by the constraint). Palestinian Arab residents (powerless, constrained exit, losing land) sit at d=0.65-0.75 (targets bearing costs despite theoretical binational status). Hebrew cultural movement (organized power, mobile exit, controls agenda) sits at d=0.15 (beneficiary/setter). Arab nationalists are EXCLUDED rather than positioned on the d axis — they cannot exit into an alternative frame; the constraint forecloses their sovereignty position as incompatible with Jewish cultural-political presence. The directionality asymmetry (beneficiaries with exit options vs. powerless constrained payers) is the structural fact the reading cannot contain: genuine cultural exchange requires non-coercive participation, but the reading's settlement logic operates through land purchase and demographic displacement that powerless Arabs cannot freely choose to enter.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (diaspora spiritual crisis, cultural assimilation threat to Jewish identity) is LIVE at the reading's inception (1880-1900) and its cultural solution is GENUINE. However, by 1920-1948, the founding problem is increasingly ADDRESSED — Hebrew culture is reviving, institutions are thriving, Jewish intellectual life has demonstrably renewed. Yet the constraint PERSISTS and INTENSIFIES (extractiveness stable, suppression rising, theater rising). This is mandatrophy progression: the primary justification (cultural renewal) succeeds, but the settlement machine it powered continues advancing, now driven by political and territorial logic the reading does not acknowledge. By 1935-1948, the measured theater_ratio spike (0.08→0.22) shows performative emphasis on cultural achievements increasing precisely as the constraint's real function (territorial consolidation) becomes undeniable. The reading's own axioms do not justify this persistence; it has become instrumentally decoupled from its founding problem. A clean cultural Zionism reading would have declared success by 1920 and stood aside for Arab self-determination; its continuation through 1948 requires political justifications (Jewish statehood, antisemitism response) it explicitly rejects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_zionism_political_inevitability,
    'Was cultural Zionism a genuinely distinct political alternative, or was it always the precursor phase of political Zionism — a rational-seeming bridge that led structurally and necessarily to territorial statehood?',
    'Counterfactual historical analysis: if the reading''s own premises had been enforced (cultural autonomy without territorial sovereignty, genuine binational governance, consent-based Arab participation), would a stable equilibrium have held? Alternatively: did the reading''s own institutional logic (land purchase, population growth, institutional dominance) necessarily generate the political competition it did not acknowledge?',
    'If cultural Zionism was a genuine alternative, it represents a foreclosed possibility and the reading retains analytical credibility as a suppressed framework. If it was inevitable precursor to political Zionism, the reading is a revealed-preference rationalization and should be reclassified as ideological cover (high theater, effective snare from Arab seats). This determines whether the reading was foreclosed by history or by its own logic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_zionism_political_inevitability, conceptual, 'Whether cultural Zionism was an independent alternative or structurally entailed political expansion.').

omega_variable(
    binational_coexistence_feasibility,
    'Could the reading''s vision of binational cultural institution with Arab political parity actually function given the structural power asymmetry (Jewish capital/organization + Arab powerlessness + Ottoman/Mandate authority favoring Jewish settlement)?',
    'Examine historical moments when binational governance was proposed or attempted (1920s Zionist-Arab discussions, 1947 UN partition alternatives, contemporary comparisons to other binational or consociational systems). Document the concrete breakdown points: could decision-making have been genuinely joint, or would Jewish institutional dominance have been structurally inevitable?',
    'If feasible, the reading''s axioms are holdable and the divergence between the reading and political Zionism is real. If infeasible (governance collapsed to Jewish control, Arab voice marginalized despite formal parity), the reading''s binational premise is itself theatrically maintained and the constraint should be reclassified from rope to tangled_rope or snare from Arab seats. This determines the reading''s internal coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binational_coexistence_feasibility, empirical, 'Whether the reading''s binational structure could have been institutionally sustained.').

omega_variable(
    arab_agency_and_consent,
    'To what extent did the reading''s operation depend on Arab SILENCE or NON-PARTICIPATION rather than genuine consent and participation? Did Arabs opt into the reading, or was their ''acceptance'' actually powerlessness combined with exclusion from alternative frameworks?',
    'Examine Arab voices contemporary to the constraint''s operation (1880-1948): articulate rejections, conditional acceptances, absent participation in cultural institutions, land-sale decisions driven by economic desperation vs. ideological alignment. Distinguish between willing adoption of the reading and passive non-resistance due to lack of power to resist.',
    'If Arabs actively consented and participated, the reading''s beneficiary claim is grounded and the constraint is genuinely coordinating. If Arab quiescence was structural powerlessness rather than consent, the constraint extracts consent through asymmetric power (higher effective extraction, higher theater, reclassify from rope toward tangled_rope or snare from Arab seats). This determines whether beneficiary status is real or imposed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arab_agency_and_consent, empirical, 'Whether Arab participation in the reading was voluntary or structurally compelled by power asymmetry.').

omega_variable(
    reading_axiom_foreclosure_by_siblings,
    'Do the political Zionism and revisionist Zionism readings FORECLOSE the cultural Zionism axioms, or do all three coexist as live positions held by different parties?',
    'Examine whether a single framework (e.g., a unified Zionist movement, a coherent international commitment, a single institutional structure) could simultaneously hold cultural Zionism premises and political Zionism premises, or whether holding one requires explicitly rejecting the other.',
    'If foreclosed: political Zionism''s victory (1948 statehood) logically eliminated cultural Zionism as a coherent framework — the reading is analytically defeated. If coexisting: cultural Zionism remains a live position within the broader Zionist spectrum despite political Zionism''s institutional dominance — the reading''s axioms could be revived. This determines whether the reading is a historical road-not-taken or a permanently superseded position.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_axiom_foreclosure_by_siblings, conceptual, 'Logical and historical relationship between cultural Zionism and its sibling political readings.').

omega_variable(
    suppression_mechanism_structural_or_internalized,
    'Is the measured suppression (0.35 at interval end, rising from 0.15) STRUCTURAL (Ottoman/Mandate authorities and Jewish organizational power preventing Arab nationalist expression) or INTERNALIZED (Arabs accepting the constraint''s framing of cultural coexistence as inevitable or desirable, carrying the suppression forward even in contexts where structural barriers weakened)?',
    'Post-1948 empirical test: did Arab populations in former British Mandate regions abandon the cultural Zionism reading immediately (structural suppression ended, frame rejected) or did some continue accepting binational or coexistence premises (internalized adoption)? Distinguish between strategic silence (tactical acceptance while organizing alternative) and genuine ideological adoption.',
    'If structural: the suppression is a property of the constraint''s operation during the interval, and the rising theater ratio accurately reflects increasing coercion required to maintain the reading against mounting resistance. If internalized: the constraint''s cultural content may have genuinely shaped some Arab preferences, and the reading''s bridge-building claims have partial validity. This affects whether suppression persists after the constraint''s formal structures are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_or_internalized, empirical, 'Whether suppression of Arab nationalist alternatives is external coercion or internalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_territorial_claim__cultural_zionism_reading, 1880, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1900, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1900, 0.12).
narrative_ontology:measurement(jewi_tr_t1920, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(jewi_tr_t1935, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1935, 0.22).
narrative_ontology:measurement(jewi_tr_t1945, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_territorial_claim__cultural_zionism_reading, theater_ratio, 1948, 0.22).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1880, 0.25).
narrative_ontology:measurement(jewi_be_t1900, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1900, 0.31).
narrative_ontology:measurement(jewi_be_t1920, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1920, 0.38).
narrative_ontology:measurement(jewi_be_t1935, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1935, 0.42).
narrative_ontology:measurement(jewi_be_t1945, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1945, 0.4).
narrative_ontology:measurement(jewi_be_t1948, jewish_territorial_claim__cultural_zionism_reading, base_extractiveness, 1948, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1880, 0.15).
narrative_ontology:measurement(jewi_su_t1900, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1900, 0.22).
narrative_ontology:measurement(jewi_su_t1920, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1920, 0.31).
narrative_ontology:measurement(jewi_su_t1935, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1935, 0.35).
narrative_ontology:measurement(jewi_su_t1945, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1945, 0.37).
narrative_ontology:measurement(jewi_su_t1948, jewish_territorial_claim__cultural_zionism_reading, suppression_requirement, 1948, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_territorial_claim__cultural_zionism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_territorial_claim__cultural_zionism_reading, 0.12).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__political_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__labor_zionism_reading).
narrative_ontology:affects_constraint(jewish_territorial_claim__cultural_zionism_reading, jewish_territorial_claim__revisionist_zionism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the jewish_territorial_claim kernel. All four readings (cultural, political, labor, revisionist Zionism) decompose the same underlying contested commitment — Jewish territorial presence and role in Palestine — into structurally distinct constraints with different beneficiary/victim structures, suppression profiles, and axioms. Cultural Zionism reading differs from political Zionism in rejecting sovereignty and majority as necessary; differs from labor Zionism in decoupling cultural renewal from socialist transformation and class struggle; differs from revisionist Zionism in rejecting maximalist territorial claims and military coercion. Each reading is authored as an independent constraint with its own ε-invariance; the sibling links (reading_relations in cs_structure) declare how they logically relate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_territorial_claim__cultural_zionism_reading, powerless, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
