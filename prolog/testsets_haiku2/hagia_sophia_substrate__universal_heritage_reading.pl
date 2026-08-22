% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_substrate__universal_heritage_reading, []).

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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Secular Universal Heritage (Museum Framing)
 *   domain: cultural/religious/political
 *
 * SUMMARY:
 *   The Hagia Sophia substrate is a contested kernel. This constraint story
 *   instantiates the UNIVERSAL HERITAGE READING: the site's legitimacy
 *   derives from its status as shared human cultural heritage transcending
 *   any single religious or national claim. Under this reading, the site is
 *   administered by Turkish secular state authority (as museum, later
 *   nominally-mosque) with the interpretive frame that its significance is
 *   world-historical and belongs to no single religious tradition. The
 *   reading emerged as the operative frame in 1934 under the Turkish
 *   secularization project and was institutionalized through UNESCO
 *   designation (1985) and museum administration (1934–2020). The global
 *   tourism and heritage scholarship sectors benefit directly from this
 *   framing; secularist Turkish elites benefit from the ideological signal of
 *   secular modernity and state authority over contested space. Islamic
 *   practitioners are suppressed — prayer is discouraged or barred, and
 *   Islamic claims to continuous worship and waqf authority are delegitimized
 *   in favor of heritage transcendence. Orthodox Christians, who claim
 *   restitution based on Byzantine origins, are similarly suppressed. The
 *   constraint persists through enforcement (architectural modification,
 *   access control, interpretive monopoly) and through normalization — the
 *   universal heritage framing is widely treated as obvious/natural,
 *   obscuring the choices that sustain it. Measurement data shows rising
 *   extractiveness (0.38 → 0.72 over the interval) and rising suppression
 *   requirement (0.42 → 0.68), indicating the constraint grew more extractive
 *   and required more active enforcement as competing claims intensified
 *   (Islamic resurgence in Turkey, global Orthodox activism, decolonial
 *   scholarship). Theater ratio rises (0.28 → 0.54), indicating growing
 *   performative maintenance of the heritage narrative even as underlying
 *   suppression intensified.
 *
 * KEY AGENTS:
 *   - global_tourism_sector: Primary beneficiary (revenue flows through hotels, guides, international visitor economy)
 *   - secularist_turkish_elites: Secondary beneficiary (ideological signal of state modernization, sovereignty over contested space)
 *   - international_heritage_scholarship: Beneficiary (access to undisturbed research site, institutional authority through UNESCO/ICOMOS frameworks)
 *   - islamic_worship_practitioners: Primary victim (suppressed prayer access, delegitimized waqf claims, interpersonal suppression)
 *   - orthodox_restitution_claimants: Secondary victim (restitution claims delegitimized, competing religious authority suppressed)
 *   - turkish_state_apparatus: Agenda-setter (controls interpretation frame, administers access, enforces the heritage narrative through law and practice)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.54).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Secular Universal Heritage (Museum Framing)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural/religious/political").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, '9956e126-7ec6-454e-9157-1603511ed9cc').
narrative_ontology:cs_kernel_codification('9956e126-7ec6-454e-9157-1603511ed9cc', fixed_text).
narrative_ontology:cs_authority_grounding('9956e126-7ec6-454e-9157-1603511ed9cc', extraction).
narrative_ontology:cs_interpretation_layer_present('9956e126-7ec6-454e-9157-1603511ed9cc').
narrative_ontology:cs_reading_relation('9956e126-7ec6-454e-9157-1603511ed9cc', hagia_sophia_substrate__islamic_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('9956e126-7ec6-454e-9157-1603511ed9cc', hagia_sophia_substrate__orthodox_restitution_reading, coexists_with).
narrative_ontology:cs_axiom('9956e126-7ec6-454e-9157-1603511ed9cc', foundational, heritage_transcends_sectarian_claim).
narrative_ontology:cs_axiom_status(heritage_transcends_sectarian_claim, holdable).
narrative_ontology:cs_axiom_grounding('9956e126-7ec6-454e-9157-1603511ed9cc', heritage_transcends_sectarian_claim, conventional).
narrative_ontology:cs_axiom('9956e126-7ec6-454e-9157-1603511ed9cc', secondary, secular_administration_neutrality).
narrative_ontology:cs_axiom_status(secular_administration_neutrality, overridden).
narrative_ontology:cs_axiom_grounding('9956e126-7ec6-454e-9157-1603511ed9cc', secular_administration_neutrality, deontological).
narrative_ontology:cs_reference_frame('9956e126-7ec6-454e-9157-1603511ed9cc', universal_heritage_transcendence).
narrative_ontology:cs_drift_state('9956e126-7ec6-454e-9157-1603511ed9cc', contemporary_post_2020_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('9956e126-7ec6-454e-9157-1603511ed9cc', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, international_heritage_scholarship).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_practitioners).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secular_heritage_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hotels, travel agencies, tour operators, and cultural tourism industries profit from the site's UNESCO designation and heritage status. They benefit from stable, predictable access; from interpretive narratives that position the site as world-historical treasure; and from architectural preservation that maintains visual and cultural value. They have arbitrage options: if the heritage frame collapses, capital flows to other heritage sites. But while heritage status holds, they capture substantial revenue.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector, beneficiary,
    institutional, generational, arbitrage, global).

% Turkish political and intellectual elites who embrace secular modernization benefit from the universal heritage framing as an ideological signal: the site demonstrates that Turkey is modern, cosmopolitan, and in control of its contested symbols. The frame also symbolizes state sovereignty and authority over space that Islamic constituencies might claim. They bear enforcement costs (diplomatic pressure, internal resistance, need to manage the narrative as resistance grows) but gain authority and ideological confirmation.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites, agenda_setter).

% Academic disciplines (archaeology, art history, conservation science, architectural history) benefit from the site's status as transcendent heritage accessible for disinterested scholarship. The universal heritage framing provides institutional authority (UNESCO frameworks, conservation protocols, peer-reviewed access) that privileges their disciplinary methods over religious or nationalist readings. They have some exit options (research at other heritage sites) but benefit from the site's uniqueness and institutional openness.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, international_heritage_scholarship, beneficiary,
    organized, generational, mobile, global).

% Islamic communities with continuous claim to the site through Ottoman waqf and 1453 onwards worship practice. Under the universal heritage reading, their prayer is suppressed (barred or heavily discouraged during museum phase), their waqf authority is delegitimized, and their claim to sacred use is reframed as incompatible with heritage transcendence. They cannot exit this claim — the site is central to Islamic identity and Turkish Islamic heritage. They are trapped by architectural modifications (conversions to museum use), legal frameworks (Turkish secular constitutional law privileging heritage over religious endowment), and institutional exclusion (international heritage protocols that treat religious use as contamination). The 2020 conversion was a reversal of this constraint, not a resolution — it exposed the constraint's underlying asymmetry by showing that secular heritage and Islamic worship are actually competing uses, not coordinated ones.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_practitioners, payer,
    organized, generational, identity_locked, national).

% Orthodox Christian constituencies (Greek state, Russian Orthodoxy, Ecumenical Patriarch, Diaspora communities) claim the site should return to ecclesiastical control based on Byzantine founding (537 CE) and argue its status as a cathedral precedes Islam. Under the universal heritage reading, their restitution claims are delegitimized as nationalist or sectarian, incompatible with transcendent heritage status. They have constrained exit (litigation in Turkish courts, diplomatic advocacy) but cannot physically claim or control the site. Their claim is suppressed as incompatible with the heritage frame even while they are presented as having 'equal access' (tourism, non-worship visiting).
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_restitution_claimants, payer,
    moderate, generational, constrained, global).

% The Turkish state sets and enforces the universal heritage interpretation through constitutional law (secularism principle), museum administration (1934–2020), and international engagement (UNESCO, ICOMOS). The state benefits from control over contested space and ideological modernization signal, but bears enforcement costs: diplomatic pressure (especially post-2020 from Orthodox and secular heritage advocates), internal religious resistance, and need to maintain the heritage narrative against competing claims. The 2020 conversion shows the state's constrained exit — even the state cannot credibly exit the constraint's logic without losing face, yet the constraint's underlying contradictions forced a reversal.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% International secular and scientific advocates of heritage preservation benefit from the universal framing. They argue the site's significance is architectural, historical, and aesthetic — not religious or nationalist. They benefit from stable, secular governance that privileges preservation and research access over religious practice. They have some exit (can advocate at other sites) but align with the institutional frameworks (UNESCO, conservation science) that the universal framing stabilizes.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secular_heritage_advocates, beneficiary,
    organized, biographical, mobile, global).

% Orthodox ecclesiastical leadership (Ecumenical Patriarch in Istanbul, Russian Orthodox church, Greek Orthodox church) would have substantial things to say about the site's status and future if they were at the table as equal parties. They are structurally excluded by the universal heritage reading: their claims are characterized as sectarian or nationalist, incompatible with transcendent status. If included as parties (not tourists), they would argue for restitution or neutral sacred status. Their exclusion is enforced through the heritage frame itself — the frame treats religious authority as incompatible with universal significance.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_orthodox_leadership, excluded,
    institutional, generational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, global_tourism_sector).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes the contested site's status by treating it as transcending any single religious or national claim. Coordinates among multiple constituencies (tourists, scholars, secular states, heritage organizations) around a shared interpretive frame that promises equal access and preservation. Solves the coordination problem of managing a site with multiple historical identities (Byzantine Christian, Ottoman Islamic, modern secular) by subordinating all to heritage transcendence.
% TRANSFER_FUNCTION: Moves legitimacy (authority to speak about and control the site), revenue (tourism income), and institutional authority (UNESCO designation, conservation protocols, scholarly prestige) from religious stakeholders and Ottoman-era endowment structures to secular state apparatus, international heritage organizations, and tourism sectors. Moves interpretive monopoly away from Islamic waqf and Orthodox ecclesiastical authority toward technocratic museum/heritage administration.
% ABSENT_VOICES: Islamic worship practitioners and Orthodox ecclesiastical leadership would object loudly if treated as equal parties rather than tourists or heritage observers. Islamic constituencies argue the site is continuous Islamic worship space and waqf-endowed. Orthodox argue it should return to ecclesiastical control or remain neutral to honor Byzantine origins. These voices are present (Turkey has Islamic leadership, Orthodox churches exist globally) but are structurally excluded from the frame — treated as sectional interests incompatible with transcendent heritage status rather than as legitimate claimants to the site's future.
% DISAPPEARANCE_RATIONALE: If the universal heritage reading vanished (if the Turkish state and international community ceased treating it as transcendent heritage), the site would immediately be reframed. It would become either an Islamic worship space (per the sovereignty reading) or contested between Islamic and Orthodox claims. Revenue flows would shift (pilgrimage vs. tourism), governance would shift (religious authority vs. secular administration), access would shift (worshippers vs. tourists), and architectural use would shift (active mosque/church vs. museum preservation). The world rearranges because the constraint's only function is interpretive — it has no material existence independent of the narrative frame that sustains it.
% FOUNDING_PROBLEM: In 1934, the newly founded Turkish secular state inherited a site that was simultaneously sacred to Ottoman Islam (nearly 500 years of continuous waqf and worship) and symbolically central to Turkish identity (but now in a state committed to secularism). The site was also deteriorating and needed funding. The founding problem was: how can a secular state legitimately control and fund a site claimed by its Islamic heritage without either (a) endorsing religious authority or (b) appearing to suppress Islamic claims? The universal heritage solution: treat the site as beyond any single claim, designate it as museum/heritage, and appeal to international heritage norms.
% FOUNDING_PROBLEM_CORROBORATION: Turkish secular state and international heritage organizations (UNESCO, ICOMOS) attest the problem was live and the universal solution was appropriate. Turkish Islamic leadership and Orthodox claimants attest the problem was never what the state claimed — the real problem was the state's desire to suppress Islamic authority while appearing modern. Post-2020 conversion by Erdoğan and global Islamic resurgence attest the problem was unresolved (religious claims never went away) and the solution was always unsustainable. Academic historians (outside heritage-preservation circles) document that the 1934 decision was explicitly political (Atatürk's secularization project) not heritage-preservation driven. The problem's status is contested: was it a genuine coordination problem, or was it an ideological instrument to suppress religious claims? The answer depends on which reading's authority one accepts.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hagia_sophia_substrate__universal_heritage_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hagia_sophia_substrate__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the universal heritage framing concentrates legitimacy (and revenue, institutional authority) among secular/tourism beneficiaries while suppressing competing religious claims. It is NOT extractive in the sense that tourism or scholarship are bad — it is extractive in that the constraint asymmetrically benefits one set of uses (secular tourism) while imposing costs on another (religious practice). The suppression measurement (0.68) is high because the constraint requires continuous active defense: prayer must be discouraged, architectural elements must be modified, interpretive monopoly must be maintained against competing historical readings. Theater ratio (0.54) reflects the performative character of 'universal heritage' — the framing is staged through museum aesthetics, interpretive panels, and international heritage protocols, and increasingly performs heritage-ness even as functional suppression of religious practice increases. The temporal trajectory is crucial: extractiveness rises as global Islamic resurgence and Orthodox activism intensify competing claims. The universal heritage reading must work harder (higher theater, higher suppression requirement) to maintain its framing as the constraint's baseline challenge grows. Accessibility collapse (0.48, moderate-low) reflects that alternatives remain partially available — Islamic prayer happens at risk, Orthodox restitution is litigated in Turkish courts, secular heritage access is robust — but no alternative has consolidated institutional authority comparable to the heritage frame. Resistance (0.71, high) reflects the real, organized opposition from Islamic and Orthodox constituencies, though the resistance has not yet succeeded in reframing the site. The claim/metric gap is intentional: this reading CLAIMS tangled_rope (coordination of plural heritage interests + extraction of Islamic/Orthodox suppression), and the metrics describe a constraint that is increasingly extractive and theatrically maintained. The engine computes whether that claim holds.
 *
 * PERSPECTIVAL GAP:
 *   The tourism/heritage beneficiary seat and the Turkish state seat should compute near-beneficiary end (low directionality): they collects gains (revenue, authority, ideological modernization signal) without bearing suppression costs. The Islamic and Orthodox victim seats should compute near-target end (high directionality): they bear suppression costs (prayer barriers, interpretive delegitimization, institutional exclusion) without collecting architectural/heritage benefits — the benefits they would collect (restitution, liturgical authority, sacred use) are precisely what the constraint suppresses. The state agenda-setter seat is complex: it benefits from the ideological frame and revenue, but also bears enforcement costs (maintaining the narrative against rising resistance, managing international criticism post-2020). The global tourism sector is cleanest beneficiary — high revenue, zero suppression cost. This seat divergence should be sharp.
 *
 * DIRECTIONALITY LOGIC:
 *   BENEFICIARIES: global_tourism_sector (revenue from heritage designation, unconstrained access, architectural preservation for visual consumption), secularist_turkish_elites (ideological signal of modernity and state authority over contested space, institutional control), international_heritage_scholarship (undisturbed research access, institutional authority through UNESCO/ICOMOS frameworks that privilege their disciplinary methods). These agents receive structural flows (money, authority, legitimacy, access) from the constraint without bearing its suppression costs. Directionality for this group trends toward 0.0 (full beneficiary). VICTIMS: islamic_worship_practitioners (prayer suppressed, waqf claims delegitimized, institutional access barred, community identity suppressed), orthodox_restitution_claimants (restitution claims treated as illegitimate, competing religious authority suppressed). These agents bear the suppression costs directly. Their exit is constrained (they cannot relocate their sacred site, cannot make claims in the international heritage framework, are trapped by the architectural and legal changes that institutionalized the heritage reading). Directionality for this group trends toward 1.0 (full target). SECONDARY POSITIONS: Turkish state apparatus sits at agenda_setter (sets and enforces the frame, collects some revenue) but also bears enforcement costs (diplomatic costs, internal resistance, need to stage performances of heritage commitment as resistance grows). Directionality slightly above 0.5, toward beneficiary — the state's strategic interest in the frame outweighs its enforcement burden.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: in 1934, the Hagia Sophia was deteriorating, religiously contested, and symbolically contested. The secular state needed to resolve its status. The universal heritage framing was an ingenious solution: by treating it as transcending any single claim, the state could claim authority while appearing neutral. The founding problem (contested sacred site threatening social cohesion) is now DEAD in the narrow sense — the site is stable and controlled. But the constraint persists in increasingly extractive form: it now primarily functions not to resolve the site's contested status but to suppress Islamic sovereignty claims and Orthodox restitution while maintaining revenue streams and ideological signaling for secular elites. The 2020 conversion decision (Erdoğan converting the museum back to active mosque) exposed the underlying asymmetry: when the political conditions shifted, the universal heritage reading collapsed not because it resolved the problem (it hadn't — the problem of competing claims persisted underneath the heritage performance) but because it primarily served to suppress one party's claims. This is a textbook mandatrophy case: founding problem dead, but the constraint persists because it now serves extraction rather than coordination. The engine should flag this for reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    heritage_universalism_versus_religious_practice,
    'Does the ''universal heritage'' framing genuinely accommodate pluralistic religious practice, or does it suppress religious practice to protect tourist/secular access?',
    'Compare the constraint''s treatment of (1) non-destructive Islamic prayer, (2) Orthodox liturgy, and (3) secular cultural access; measure suppression cost differential across these three uses.',
    'If suppression is asymmetric (prayer suppressed, tourist access unimpeded), the universal framing is revealed as selective — benefiting heritage/tourism while extracting from religious communities. If symmetrically managed, the coordination framing is more credible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(heritage_universalism_versus_religious_practice, empirical, 'Whether universal heritage actually coordinates plural uses or suppresses non-secular uses.').

omega_variable(
    kernel_reading_identity_ambiguity,
    'Is this the ''universal heritage'' reading a coherent normative position, or does it function primarily as the political instrument through which secularist elites suppress Islamic sovereignty and Orthodox restitution claims?',
    'Historical analysis of the reading''s adoption (1934 secularization decision), elite composition of UNESCO/museum governance, and whether the same framing is applied to contested religious sites in other jurisdictions (Aqsa, Varanasi, Jerusalem) or only selectively to Hagia Sophia.',
    'If the reading is instrumentalized (applied selectively to suppress religion in Turkey while not applied consistently globally), it is revealed as a cover for extraction, not as a genuine philosophical position. This affects whether the constraint is characterized as tangled_rope (coordination + extraction) or snare (pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity_ambiguity, conceptual, 'Whether universal heritage is a principle or an instrument for suppressing Islamic/Orthodox claims.').

omega_variable(
    suppression_internalization_interpersonal,
    'Is the measured suppression (0.68) primarily structural (legal barriers to prayer, architectural modifications, enforcement presence) or internalized (Islamic and Orthodox communities have internalized the belief that the site is not ''theirs'' and should not be claimed)?',
    'Post-legalization trajectory: if the 2020 conversion to mosque freed suppressed demand (prayer frequency, clerical involvement, devotional practices), suppression was internalized and traveled with the community even when barriers fell; if demand merely shifted use patterns without intensifying, suppression was primarily structural.',
    'Internalized suppression implies higher effective extraction and longer persistence even after enforcement machinery weakens. Structural suppression might dissolve faster if barriers are removed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_internalization_interpersonal, empirical, 'Structural versus internalized suppression of Islamic worship claims.').

omega_variable(
    reading_relation_to_sibling_islamic_sovereignty,
    'What is the logical relationship between the universal heritage reading and the Islamic sovereignty reading? Does one rule out the other, or can they coexist as competing frameworks?',
    'Doctrinal analysis: the Islamic reading grounds legitimacy in Ottoman waqf and continuous worship (1453-present); the universal reading grounds legitimacy in heritage transcendence (outside any single religious claim). In a single legal/constitutional framework (Turkish law), can both be satisfied simultaneously, or must one suppress the other?',
    'If they logically foreclose each other (a site cannot simultaneously be exclusively Islamic worship space and transcendent heritage beyond religious claim), the relationship is forecloses. If different institutional actors hold both (Turkish state authority as secular heritage steward vs. religious communities as worshippers), they coexist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_to_sibling_islamic_sovereignty, conceptual, 'Logical/structural relationship between universal heritage and Islamic sovereignty readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(hagi_tr_t18, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 18, 0.38).
narrative_ontology:measurement(hagi_tr_t36, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 36, 0.5).
narrative_ontology:measurement(hagi_tr_t54, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 54, 0.54).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hagi_be_t18, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 18, 0.55).
narrative_ontology:measurement(hagi_be_t36, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 36, 0.68).
narrative_ontology:measurement(hagi_be_t54, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 54, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(hagi_su_t18, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 18, 0.54).
narrative_ontology:measurement(hagi_su_t36, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 36, 0.64).
narrative_ontology:measurement(hagi_su_t54, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 54, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.12).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The Hagia Sophia substrate decomposes into three structurally distinct constraints, each representing a different reading of the contested kernel. The universal heritage reading (this story) grounds legitimacy in transcendent cultural status and benefits global tourism/heritage sectors while suppressing Islamic and Orthodox claims. The Islamic sovereignty reading grounds legitimacy in Ottoman waqf and continuous worship, benefiting Islamic practitioners and Turkish Islamic authority while excluding secular heritage and Orthodox claims. The Orthodox restitution reading grounds legitimacy in Byzantine founding and claims restitution or neutral status, benefiting Orthodox authority while challenging both secular heritage and Islamic sovereignty. Each reading has a distinct ε (the universal reading is highly extractive toward Islamic/Orthodox; the Islamic reading is extractive toward secular heritage; the Orthodox reading is extractive toward both); distinct beneficiary/victim structures; and distinct authority grounding. They are not different views of the same constraint — they are three different constraints instantiated from the same substrate, each with different victims and beneficiaries depending on which reading's authority is established. The ε-invariance principle requires separation: a single constraint story cannot track three different ε values depending on measurement frame (reading chosen). Each reading is its own story, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
