% ============================================================================
% CONSTRAINT STORY: hagia_sophia_substrate__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: hagia_sophia_substrate__universal_heritage_reading
 *   human_readable: Hagia Sophia as Universal Cultural Heritage (Secular Museum Framing)
 *   domain: cultural_heritage/religious_authority/sovereignty
 *
 * SUMMARY:
 *   The Hagia Sophia in Istanbul stands as a contested kernel: a structure
 *   built as a Christian cathedral (537 CE), converted to an Islamic mosque
 *   (1453), secularized into a museum (1935), and briefly returned to mosque
 *   status (2020 onward). This constraint instantiates ONE reading: the
 *   universal-heritage frame, which claims the site's legitimacy derives from
 *   its transcendence of any single religious or national claim — framing it
 *   as shared human cultural heritage stewarded by secular technocratic
 *   administration under Turkish constitutional law and UNESCO heritage
 *   systems. This reading beneficiaries the global tourism and scholarship
 *   sectors, secularist Turkish political elites, and the UNESCO heritage
 *   regime itself. It suppresses Islamic worship claims (rooted in 479 years
 *   of continuous endowment and contemporary Muslim constituencies) and
 *   Orthodox restitution claims (rooted in 900 years of Christian sacred
 *   history). The claim/metric gap is structural: the universal-heritage
 *   reading CLAIMS to transcend particularity while the authored metrics
 *   describe substantially extractive, actively enforced suppression of two
 *   religious claim-sets in service of a secular ideological frame. The
 *   engine measures this divergence.
 *
 * KEY AGENTS:
 *   - museum_administration: Agenda-setter (institutional power) — sets access rules, interpretive frame, revenue collection under Turkish constitutional authority
 *   - secularist_turkish_elites: Beneficiary (institutional power) — gains ideological capital from modernist/cosmopolitan positioning against both Ottoman Islamic and Byzantine Orthodox frames
 *   - global_tourism_scholarship_sector: Beneficiary (organized power) — gains revenue, publishing opportunities, international collaboration from universal-heritage designation
 *   - islamic_worship_constituencies: Victim, excluded (organized power, identity-locked exit) — suppressed claim to waqf endowment and continuous Islamic use; identity fused to Islamic sacred tradition
 *   - orthodox_ecclesiastical_claimants: Victim, excluded (organized power, identity-locked exit) — suppressed claim to Byzantine Christian origins and restitution; identity fused to Orthodox religious tradition
 *   - turkish_state_constitutional_framework: Agenda-setter (institutional power) — authority structure binding secular museum status and suppressing religious alternatives
 *   - unesco_heritage_system: Beneficiary (institutional power, analytical seat) — legitimacy and scope amplified by universal-heritage principle; Hagia Sophia is a linchpin case
 *   - scholars_heritage_conservators: Observer (analytical power) — can document competing claims and constraint costs but lack authority to contest Turkish constitutional sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_substrate__universal_heritage_reading, 0.68).
domain_priors:suppression_score(hagia_sophia_substrate__universal_heritage_reading, 0.72).
domain_priors:theater_ratio(hagia_sophia_substrate__universal_heritage_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hagia_sophia_substrate__universal_heritage_reading, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_substrate__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_substrate__universal_heritage_reading, "Hagia Sophia as Universal Cultural Heritage (Secular Museum Framing)").
narrative_ontology:topic_domain(hagia_sophia_substrate__universal_heritage_reading, "cultural_heritage/religious_authority/sovereignty").

domain_priors:requires_active_enforcement(hagia_sophia_substrate__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hagia_sophia_substrate__universal_heritage_reading, 'd46f819b-7fc5-4b66-b243-56243491d350').
narrative_ontology:cs_kernel_codification('d46f819b-7fc5-4b66-b243-56243491d350', fixed_text).
narrative_ontology:cs_authority_grounding('d46f819b-7fc5-4b66-b243-56243491d350', extraction).
narrative_ontology:cs_interpretation_layer_present('d46f819b-7fc5-4b66-b243-56243491d350').
narrative_ontology:cs_reading_relation('d46f819b-7fc5-4b66-b243-56243491d350', hagia_sophia_substrate__islamic_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d46f819b-7fc5-4b66-b243-56243491d350', hagia_sophia_substrate__orthodox_restitution_reading, forecloses).
narrative_ontology:cs_axiom('d46f819b-7fc5-4b66-b243-56243491d350', foundational, secular_transcendence_principle).
narrative_ontology:cs_axiom_status(secular_transcendence_principle, holdable).
narrative_ontology:cs_axiom_grounding('d46f819b-7fc5-4b66-b243-56243491d350', secular_transcendence_principle, conventional).
narrative_ontology:cs_axiom('d46f819b-7fc5-4b66-b243-56243491d350', foundational, cultural_universality_over_religious_particularity).
narrative_ontology:cs_axiom_status(cultural_universality_over_religious_particularity, holdable).
narrative_ontology:cs_axiom_grounding('d46f819b-7fc5-4b66-b243-56243491d350', cultural_universality_over_religious_particularity, deontological).
narrative_ontology:cs_reference_frame('d46f819b-7fc5-4b66-b243-56243491d350', secular_modernist_transcendence).
narrative_ontology:cs_drift_state('d46f819b-7fc5-4b66-b243-56243491d350', contemporary_post_2020_reconversion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d46f819b-7fc5-4b66-b243-56243491d350', '').
narrative_ontology:cs_kernel_id(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_framework).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_constituencies).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_ecclesiastical_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_political_elites).
narrative_ontology:constraint_beneficiary(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_system).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, orthodox_national_government_greece).
narrative_ontology:constraint_victim(hagia_sophia_substrate__universal_heritage_reading, islamic_states_and_muslim_majority_governments).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, secular_modernist_governance_doctrine).
narrative_ontology:constraint_vindicates(hagia_sophia_substrate__universal_heritage_reading, universal_cultural_property_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the site as a secular museum under Turkish constitutional law. Sets hours, admission fees, interpretive framing, and access rules. Presents the constraint as preserving universal human heritage and advancing Turkey's integration into global secular modernity. Collects revenue from ticket sales and leverages the site's world-heritage status for institutional prestige and international scholarly networking.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, museum_administration, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the universal-heritage framing as proof of Turkey's secular, modern, cosmopolitan state identity — in contrast to both Ottoman Islamic authority and Byzantine Orthodox authority. The site becomes a symbol of nationalist secularism: transcending religious particularity is presented as the nation's civilizational achievement. No direct extraction, but significant ideological gain.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, secularist_turkish_political_elites, beneficiary,
    institutional, generational, mobile, national).

% Academic institutions, heritage conservation organizations, tourism boards, and international cultural agencies benefit from the site's designation as universal human heritage. It generates scholarly publishing opportunities, UNESCO recognition, heritage tourism flows, and international collaboration frameworks. The designation legitimizes treating the site as a common resource available to global inquiry rather than a contested religious space.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, global_tourism_scholarship_sector, beneficiary,
    organized, biographical, mobile, global).

% Bear the cost of suppressed worship claims. Muslim constituencies (domestic and diaspora) maintain that the site remains an Islamic endowment (waqf) under Islamic law and should serve Islamic worship — a claim systematically overridden by the museum framing. Identity is fused to Islamic tradition and the site's history as a mosque for 479 years; exit would mean accepting permanent displacement from a space sacred to their tradition.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_constituencies, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, islamic_worship_constituencies, excluded).

% Bear the cost of suppressed restitution claims. Orthodox Christian communities (Ecumenical Patriarchate, diaspora congregations) maintain that the site is their ancestral cathedral, built and consecrated in their tradition, and should return to Orthodox ecclesiastical use or neutral status honoring its Christian origins. Their identity is fused to this space as a foundational site of Orthodox Christianity; exit would mean permanent dispossession.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_ecclesiastical_claimants, payer,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, orthodox_ecclesiastical_claimants, excluded).

% The secular Turkish constitution and its legal regime provide the authority structure for the museum framing. The constraint persists because Turkish law and international treaty obligations (UNESCO World Heritage Convention) establish the secular cultural heritage reading as binding. This constitutive authority keeps alternative readings (Islamic sovereignty, Orthodox restitution) off the operating table.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, turkish_state_constitutional_framework, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains legitimacy and operational scope from sites like Hagia Sophia that embody the universal-heritage principle. The system's authority to designate and protect world heritage depends on sites being frameable as transcending national and religious particularity. Hagia Sophia is a linchpin case; its status strengthens UNESCO's claim to global cultural stewardship.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, unesco_heritage_system, beneficiary,
    institutional, generational, analytical, global).

% Greece maintains diplomatic pressure for Orthodox restitution but has limited leverage; it cannot directly contest Turkish sovereignty over the site. It benefits somewhat from the universal-heritage framing (intellectual property in Orthodox heritage claims), but bears the cost of permanent suppression of its ecclesiastical constituency's claims. Its position is constrained by geopolitical asymmetry.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, orthodox_national_government_greece, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, orthodox_national_government_greece, observer).

% Maintain that the site should serve Islamic worship but have limited leverage to alter Turkish state policy. Some file UNESCO objections or diplomatic protests; none can effectively contest Turkish constitutional authority. The constraint's persistence depends partly on their inability to mount coordinated alternative framing.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, islamic_states_and_muslim_majority_governments, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hagia_sophia_substrate__universal_heritage_reading, islamic_states_and_muslim_majority_governments, observer).

% Analytical seat: historians, conservation experts, and heritage professionals study the site's technical condition, historical record, and competing claims. They can document the cost structure of the constraint (which voices are suppressed, what evidence exists for each reading) but do not directly contest the Turkish state's authority to define the site's status.
narrative_ontology:constraint_stakeholder(hagia_sophia_substrate__universal_heritage_reading, scholars_heritage_conservators, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hagia_sophia_substrate__universal_heritage_reading, museum_administration).
narrative_ontology:fixing_cost_class(hagia_sophia_substrate__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the site's physical integrity and makes it accessible to global scholarly study, tourism, and heritage conservation — unified curatorial stewardship under international (UNESCO) oversight, rather than contested religious use or competing national claims fragmenting access and maintenance.
% TRANSFER_FUNCTION: Transfers authority to define the site's meaning and use from religious communities (Islamic waqf, Orthodox church hierarchy) to a secular state-administered museum under international heritage law. Simultaneously transfers economic benefit (ticket revenue, tourism infrastructure, academic prestige) to global tourism and scholarship sectors and secularist political elites, while suppressing Islamic worship claims and Orthodox restitution claims.
% ABSENT_VOICES: Islamic worship constituencies and Orthodox ecclesiastical authorities are structurally excluded — they would contest the universal-heritage framing as false neutrality masking secular displacement of their legitimate claims. They maintain that 'shared human heritage' erases the site's continuous religious identity and converts suppression into preservation. Their exclusion is enforced by the Turkish constitutional framework and UNESCO's heritage system, which treat the secular museum status as the default and competing religious claims as particularism.
% DISAPPEARANCE_RATIONALE: If the universal-heritage constraint vanished, the site would be re-contested between Islamic worship constituencies (claiming waqf endowment continuity), Orthodox ecclesiastical claimants (claiming restitution), and Turkish nationalist frames (claiming sovereign secular authority). One of the three readings would likely become operationally binding; the site would cease functioning as a global-heritage commons and would reorganize around one community's exclusive or primary claim.
% FOUNDING_PROBLEM: The Hagia Sophia site required stewardship after the 1935 conversion from mosque to museum under Atatürk's secularization; the challenge was how to treat a site sacred to multiple traditions without privileging any single religious claim. The universal-heritage framing proposed that neutrality and international stewardship transcended particularism.
% FOUNDING_PROBLEM_CORROBORATION: Museum administration and UNESCO attest the founding problem is live: the site requires unified stewardship to prevent conflicting uses. Islamic constituencies and Orthodox claimants attest the founding problem was solved in 1935 by secularist force, not by genuine transcendence — they dispute that the problem ever required suppressing their claims rather than negotiating shared or rotating access. Independent historians (outside the benefiting parties) document that the 1935 decision was explicitly a modernist, secularist political choice, not a neutral technical solution. No outside party attests to the universal-heritage framing's claim of transcending particularity; all three reading communities acknowledge it as one reading among contested alternatives.
narrative_ontology:disappearance_verdict(hagia_sophia_substrate__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(hagia_sophia_substrate__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hagia_sophia_substrate__universal_heritage_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hagia_sophia_substrate__universal_heritage_reading, 'none', 1).

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
 *   Extractiveness is high (0.68) and rising because the constraint transfers meaning-making authority and economic benefit (tickets, international prestige) from religious communities to secular administrators and global tourism/scholarship sectors, while suppressing the other claim-sets' capacity to access the site. The measurement series shows steady extraction increase from 0.42 (1935 transition) to 0.68 (contemporary), reflecting accumulated tourism revenue capture and ideological entrenchment of the secular frame. Suppression is high (0.72) because the constraint's persistence depends on ACTIVE enforcement: Turkey must maintain constitutional prohibition on religious worship use, UNESCO must uphold heritage designation over restitution claims, and the site's interpretive apparatus must systematically frame Islamic and Orthodox claims as particularism vs. universal transcendence. Theater is moderate-high (0.58) because a growing share of the site's operation is devoted to symbolic assertion of secular modernity (narratives about transcendence, international cooperation, heritage preservation) rather than any functional coordination problem requiring suppression. The suppression_requirement series shows rising enforcement burden (0.55→0.72) because as Islamic and Orthodox constituencies have mobilized pressure for alternative framings (especially after 2020 when Turkey reconverted the site to mosque use for Friday prayers), the universal-heritage reading has required more active defense in international forums, UNESCO processes, and Turkish state policy.
 *
 * PERSPECTIVAL GAP:
 *   The museum administration and secularist Turkish elites compute their situation as genuine coordination (preserving the site's integrity for humanity) and would classify as rope. Islamic worship constituencies and Orthodox claimants compute the identical structure as enforced suppression of their legitimate authority and would classify as snare. The UN/UNESCO analytical seat might compute as tangled rope (the coordination function is real but suppresses alternative use-claims systematically). The engine computes per-seat classification from power, exit options, and beneficiary/victim declarations — the authored claim reflects the universal-heritage reading's self-understanding; the metrics reflect what the constraint structurally does (suppress and extract). This gap is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Museum administration and secularist Turkish elites sit at low d (0.15-0.25): they control the rules, collect revenue/ideological gain, and hold institutional power and mobile exit options (they can shift the site's status if political conditions change). Global tourism/scholarship sits at symmetric-to-beneficiary (0.3-0.4): genuine coordination gain (access to a preserved site), no direct extraction beyond participation in a system that suppresses others. Islamic constituencies and Orthodox claimants sit at high d (0.8-0.9): they are the targets of suppression, have identity-locked exit (cannot walk away from a site sacred to their tradition), and hold no control over rules. The constraint's persistence depends on keeping these targets trapped and their exit options closed.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents the core mandatrophy question: the universal-heritage reading was founded to solve a real problem (how to steward a sacred site shared by multiple traditions without privileging one), but the founding mandate has atrophied into ideological cover story for secular state authority. The measurement series documents this drift: extractiveness and theater rising in tandem while suppression increases, indicating the constraint increasingly serves extraction (revenue, ideological positioning) rather than coordination (site preservation, access accommodation). The founding_problem_status=contested because Islamic and Orthodox constituencies dispute that the problem ever required suppressing their claims. Contemporary Muslim governance (post-2020 reconversion to mosque use) attests that Islamic worship and secular heritage preservation could coexist, suggesting the mandate for total suppression is manufactured rather than necessary. The universal-heritage reading lives through theater: ever-more-elaborate narratives about transcendence masking ever-more-energetic suppression of the two claim-sets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transcendence_vs_erasure_ambiguity,
    'Is the universal-heritage framing a genuine transcendence of religious particularity, or does it erase the site''s continuous religious identity and constitute a secular displacement dressed as neutrality?',
    'Examine whether the frame preserves capacity for all three religious communities (Islamic, Orthodox, Muslim visitors/scholars) to access the site in ways compatible with their traditions, or whether it systematically forecloses worship while maintaining intellectual/touristic access. Compare the site''s current operational constraints with counterfactual models (rotating religious use, shared ecclesiastical access, sacred-secular zoning) to test whether suppression is functionally necessary or ideologically preferred.',
    'If framing is genuine transcendence, the constraint is tangled rope (coordination + necessary asymmetry). If framing is secular displacement, the constraint is snare (suppression in service of extraction). Empirical test: would the site''s heritage preservation function collapse if Islamic Friday prayers and Orthodox liturgies resumed? If not, suppression is extractive and ideological, not functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transcendence_vs_erasure_ambiguity, empirical, 'Whether the universal-heritage frame constitutes genuine neutrality or secular displacement of religious particularity.').

omega_variable(
    kernel_reading_constitutiveness,
    'Is this a reading of a single contested kernel (the Hagia Sophia structure), or are the three readings actually three different constraints (three different stabilized commitments) that happen to instantiate on the same building?',
    'Examine whether the three readings share a common ε-invariant constraint (same beneficiary/victim structure, same functional problem, different interpretive framings) or whether each reading entails fundamentally different structural constraints with incommensurable ε values. If three readings can coexist within one institutional framework (e.g., shared governance, rotating authority, negotiated access), they are readings of one kernel. If no framework could hold all three simultaneously, they are incommensurable constraints.',
    'If one kernel: the universal_heritage_reading is one reading among three live alternatives, and the constraint''s persistence depends on active suppression of alternatives. If three constraints: the universal_heritage_reading is not itself contested; rather, three incommensurable visions compete for the site, and the one currently binding is sustained by Turkish state power. This omega routes to the framing choice documented in cs_structure.reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_constitutiveness, conceptual, 'Whether the three readings are interpretations of one kernel or three distinct constraints competing for the site.').

omega_variable(
    identity_lock_mechanism_suppression,
    'For Islamic and Orthodox constituencies with identity-locked exit, is their suppression enforced by external barriers (Turkish state law, UNESCO policy, physical control of the building) or by internalized acceptance of the secular frame (cognitive capture, belief that transcendence is legitimate)?',
    'Post-exit suppression trajectory: if constituencies removed from the constraint continue to assert claims (maintain restitution movements, file UNESCO objections, organize diplomatic pressure), suppression was structural. If constituencies internalize the universal-heritage frame and cease asserting alternative claims, suppression has become partly internalized. Monitor the 2020 post-reconversion period: did constituencies gain confidence from the temporary mosque status, or did they internalize defeat?',
    'If structural, the constraint''s effective suppression is as authored (0.72). If partly internalized, the constraint''s suppression persists even if the site were returned to some form of shared or religious use, indicating ideological capture layered onto the structure. This would suggest the constraint''s true function is not site preservation but ideological commitment to secular modernity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_suppression, empirical, 'Whether suppression of religious claims is maintained by external enforcement or internalized as acceptance of secular legitimacy.').

omega_variable(
    universal_heritage_principle_feasibility,
    'Can the universal-heritage principle actually transcend particularity, or is it itself a particular cultural/political claim (secular Western modernity) masquerading as universality?',
    'Test whether the principle applies symmetrically: if a site were sacred to secular scholarship alone (e.g., Darwin''s birthplace), would religious worship claims be recognized as equally particularist and therefore suppressible? Or is universality defined post hoc to mean ''whatever allows the site to function under global institutional governance,'' which systematically privileges secular over religious frames? Examine UNESCO''s track record on sites where religious communities hold exclusive claims.',
    'If the principle is genuinely universal, it should protect all traditions'' claims equally. If it privileges secular-institutional over religious-communal claims, the universal-heritage frame is itself extractive ideology serving global technical governance, not transcendence. This would suggest the constraint''s true mandate is technocratic authority over culturally contested spaces, not neutrality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_heritage_principle_feasibility, preference, 'Whether the universal-heritage principle constitutes genuine transcendence or is itself a particular cultural claim about what counts as legitimate stewardship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_substrate__universal_heritage_reading, 0, 91).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagi_tr_t0, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(hagi_tr_t0, observed).
narrative_ontology:measurement(hagi_tr_t13, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 13, 0.42).
narrative_ontology:measurement_basis(hagi_tr_t13, observed).
narrative_ontology:measurement(hagi_tr_t26, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 26, 0.48).
narrative_ontology:measurement_basis(hagi_tr_t26, observed).
narrative_ontology:measurement(hagi_tr_t39, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 39, 0.52).
narrative_ontology:measurement_basis(hagi_tr_t39, observed).
narrative_ontology:measurement(hagi_tr_t65, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 65, 0.56).
narrative_ontology:measurement_basis(hagi_tr_t65, observed).
narrative_ontology:measurement(hagi_tr_t91, hagia_sophia_substrate__universal_heritage_reading, theater_ratio, 91, 0.58).
narrative_ontology:measurement_basis(hagi_tr_t91, observed).

% Extraction over time
narrative_ontology:measurement(hagi_be_t0, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hagi_be_t0, observed).
narrative_ontology:measurement(hagi_be_t13, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 13, 0.48).
narrative_ontology:measurement_basis(hagi_be_t13, observed).
narrative_ontology:measurement(hagi_be_t26, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 26, 0.55).
narrative_ontology:measurement_basis(hagi_be_t26, observed).
narrative_ontology:measurement(hagi_be_t39, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 39, 0.61).
narrative_ontology:measurement_basis(hagi_be_t39, observed).
narrative_ontology:measurement(hagi_be_t65, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 65, 0.67).
narrative_ontology:measurement_basis(hagi_be_t65, observed).
narrative_ontology:measurement(hagi_be_t91, hagia_sophia_substrate__universal_heritage_reading, base_extractiveness, 91, 0.68).
narrative_ontology:measurement_basis(hagi_be_t91, observed).

% Suppression requirement over time
narrative_ontology:measurement(hagi_su_t0, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(hagi_su_t0, observed).
narrative_ontology:measurement(hagi_su_t13, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 13, 0.6).
narrative_ontology:measurement_basis(hagi_su_t13, observed).
narrative_ontology:measurement(hagi_su_t26, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 26, 0.64).
narrative_ontology:measurement_basis(hagi_su_t26, observed).
narrative_ontology:measurement(hagi_su_t39, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 39, 0.68).
narrative_ontology:measurement_basis(hagi_su_t39, observed).
narrative_ontology:measurement(hagi_su_t65, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 65, 0.71).
narrative_ontology:measurement_basis(hagi_su_t65, observed).
narrative_ontology:measurement(hagi_su_t91, hagia_sophia_substrate__universal_heritage_reading, suppression_requirement, 91, 0.72).
narrative_ontology:measurement_basis(hagi_su_t91, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_substrate__universal_heritage_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(hagia_sophia_substrate__universal_heritage_reading, 0.22).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__islamic_sovereignty_reading).
narrative_ontology:affects_constraint(hagia_sophia_substrate__universal_heritage_reading, hagia_sophia_substrate__orthodox_restitution_reading).

% DUAL FORMULATION NOTE:
% The hagia_sophia_substrate kernel has three distinct constraint readings rooted in three incommensurable legitimacy claims: Islamic sovereignty (waqf endowment, Ottoman authority), Orthodox restitution (Christian founding, ecclesiastical tradition), and universal heritage (secular transcendence). These are not perspectives on one constraint — they are three structurally distinct constraints with different ε values, different beneficiary/victim sets, and incommensurable authority structures. The universal_heritage_reading suppresses the other two. Constraint family links: universal_heritage_reading affects (and suppresses) both islamic_sovereignty_reading and orthodox_restitution_reading; the three are linked via network.affects_constraints edges naming the siblings. Each sibling constraint story carries its own ε, its own beneficiary/victim structure, and its own authorization framework. The kernel is the contested substrate (the building); the readings are three competing instantiations of legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_substrate__universal_heritage_reading, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
