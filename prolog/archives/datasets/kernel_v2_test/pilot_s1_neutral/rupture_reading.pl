% ============================================================================
% CONSTRAINT STORY: rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rupture_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: rupture_reading
 *   human_readable: Script Reform as Deliberate Rupture: Ottoman/Islamic Past Severing and National Identity Construction
 *   domain: political_linguistics/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Turkish script reform (1928) represents a deliberate state policy to
 *   sever the Ottoman and Islamic past and construct a new national identity
 *   grounded in linguistic modernity and Western-oriented nationalism. Under
 *   this reading, the constraint is not a response to technical necessity
 *   (script complexity, printing incompatibility) but a calculated political
 *   choice to dispossess the Ottoman literate class, marginalize Islamic
 *   scholarly authority, and create epistemic discontinuity between pre- and
 *   post-reform identity. The reform demonstrates how a constraint can be
 *   simultaneously genuine coordination (enabling centralized state
 *   administration and universal literacy) and severe extraction (rendering
 *   entire populations' education worthless and cutting off generational
 *   textual transmission). This reading treats the rupture as intentional,
 *   not incidental, and therefore classifies the constraint as tangled_rope
 *   at best (coordination + extraction) or snare (extraction with
 *   coordination rhetoric) from the perspective of those dispossessed. The
 *   piton perspective shows how modernist ideology romanticizes the rupture
 *   as inevitable progress, rendering the political choice naturalized.
 *
 * KEY AGENTS:
 *   - Ottoman literate class (powerless/trapped): Scholars, administrators, scribes whose professional expertise and generational literacy investment become worthless; bear maximum extraction
 *   - Islamic scholarly tradition (powerless/trapped): Ulema and quranic scholars unable to maintain scriptural continuity; severing from textual foundation is the reform's design intent
 *   - Post-reform state apparatus (institutional/arbitrage): Primary beneficiary; script uniformity solves genuine coordination problems for centralized nation-state bureaucracy
 *   - Turkish linguistic nationalism (organized/mobile): Ideological movement realizing core agenda of ethnic homogeneity and rupture with Ottoman past; net beneficiary
 *   - Transitional generation (moderate/constrained): Young people forced to learn both scripts; coordinated across generations while bearing cognitive displacement
 *   - Ottoman institutional continuity (institutional/constrained): Broader Ottoman administrative and theological tradition experiencing enforced modernization that dismantles its symbolic grounds
 *   - Modernist ideology (institutional/arbitrage): Global narrative that treats script reform as evidence of civilizational progress; benefits from naturalizing contingent political choice as necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rupture_reading, 0.78).
domain_priors:suppression_score(rupture_reading, 0.82).
domain_priors:theater_ratio(rupture_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rupture_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(rupture_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(rupture_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rupture_reading, tangled_rope).
narrative_ontology:human_readable(rupture_reading, "Script Reform as Deliberate Rupture: Ottoman/Islamic Past Severing and National Identity Construction").
narrative_ontology:topic_domain(rupture_reading, "political_linguistics/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rupture_reading, 'e00074d4-5a70-4325-82a2-f22d682a3d2f').
narrative_ontology:cs_kernel_codification('e00074d4-5a70-4325-82a2-f22d682a3d2f', fixed_text).
narrative_ontology:cs_authority_grounding('e00074d4-5a70-4325-82a2-f22d682a3d2f', extraction).
narrative_ontology:cs_interpretation_layer_present('e00074d4-5a70-4325-82a2-f22d682a3d2f').
narrative_ontology:cs_reading_relation('e00074d4-5a70-4325-82a2-f22d682a3d2f', rupture_reading__continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('e00074d4-5a70-4325-82a2-f22d682a3d2f', rupture_reading__modernization_reading, influences).
narrative_ontology:cs_axiom('e00074d4-5a70-4325-82a2-f22d682a3d2f', foundational, script_change_is_deliberate_rupture).
narrative_ontology:cs_axiom_status(script_change_is_deliberate_rupture, holdable).
narrative_ontology:cs_axiom_grounding('e00074d4-5a70-4325-82a2-f22d682a3d2f', script_change_is_deliberate_rupture, empirically_contingent).
narrative_ontology:cs_axiom('e00074d4-5a70-4325-82a2-f22d682a3d2f', foundational, ottoman_identity_must_be_severed).
narrative_ontology:cs_axiom_status(ottoman_identity_must_be_severed, holdable).
narrative_ontology:cs_axiom_grounding('e00074d4-5a70-4325-82a2-f22d682a3d2f', ottoman_identity_must_be_severed, deontological).
narrative_ontology:cs_reference_frame('e00074d4-5a70-4325-82a2-f22d682a3d2f', ottoman_islamic_scriptural_continuity).
narrative_ontology:cs_drift_state('e00074d4-5a70-4325-82a2-f22d682a3d2f', contemporary, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('e00074d4-5a70-4325-82a2-f22d682a3d2f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(rupture_reading, orthographic_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rupture_reading, post_reform_state_apparatus).
narrative_ontology:constraint_beneficiary(rupture_reading, turkish_linguistic_nationalism).
narrative_ontology:constraint_victim(rupture_reading, ottoman_literate_population).
narrative_ontology:constraint_victim(rupture_reading, islamic_scholarly_tradition).
narrative_ontology:constraint_victim(rupture_reading, continuity_of_textual_patrimony).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rupture_reading, ottoman_state_apparatus).
narrative_ontology:constraint_beneficiary(rupture_reading, turkish_nationalism).
narrative_ontology:constraint_beneficiary(rupture_reading, transitional_generation).
narrative_ontology:constraint_victim(rupture_reading, ottoman_scribal_class).
narrative_ontology:constraint_victim(rupture_reading, ulema_quranic_scholars).
narrative_ontology:constraint_victim(rupture_reading, transitional_generation).
narrative_ontology:constraint_vindicates(rupture_reading, nation_state_requires_linguistic_homogeneity).
narrative_ontology:constraint_vindicates(rupture_reading, modernity_demands_script_rupture_with_past).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scribes, calligraphers, and document-writers whose entire professional practice was built on Arabic-script Ottoman Turkish. The reform renders their expertise worthless overnight. They cannot migrate the skill to new script (calligraphy is craft-embodied); they cannot find new work using old skills; they cannot teach what they know to the next generation in official channels. Trapped by state prohibition of Arabic-script education and by the fact that the reform is mandatory and total. No alternative domain exists where Arabic-script literacy is valued in the post-reform state.
narrative_ontology:constraint_stakeholder(rupture_reading, ottoman_scribal_class, payer,
    powerless, biographical, trapped, national).

% Islamic scholars and quranic reciters whose textual authority is grounded in Arabic-script transmission. The script reform cuts them off from the Qur'an's original scriptural form and from centuries of commentary written in Arabic script. They can learn new script, but doing so does not reconnect them to the theological authority they lost. Trapped by state suppression of Arabic-script Islamic education and by the fact that re-learning the old script unofficially carries social and professional cost.
narrative_ontology:constraint_stakeholder(rupture_reading, ulema_quranic_scholars, payer,
    powerless, generational, trapped, national).

% The centralizing Turkish state benefits from script uniformity for administrative efficiency, standardized education, legal codification, and bureaucratic control. Script change enables the state to standardize the curriculum, unify the bureaucracy, and reduce the literacy infrastructure costs. The state has high arbitrage: it could theoretically reverse the reform, accommodate bilingualism, or fund translation of old-script texts. It chooses not to, revealing that the extraction (severing Ottoman identity) is itself valued, not a side-effect.
narrative_ontology:constraint_stakeholder(rupture_reading, ottoman_state_apparatus, beneficiary,
    institutional, immediate, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rupture_reading, ottoman_state_apparatus, agenda_setter).

% The Turkish linguistic nationalism movement realizes its core agenda through the script reform: severing Ottoman-Turkish's Arabic and Persian vocabulary heritage and constructing a 'pure Turkish' national identity. The reform is not accidental alignment with nationalist ideology — the reform IS the ideology made policy. Nationalist intellectuals, educators, and state officials advocate for the reform as foundational to nation-building. They have mobile exit (can emigrate, can organize diaspora nationalism, can abandon the project). They choose to persist because the reform succeeds in their core mission.
narrative_ontology:constraint_stakeholder(rupture_reading, turkish_nationalism, beneficiary,
    organized, generational, mobile, national).

% Children and young adults forced to learn new script while their parents' literacy world becomes inaccessible to them. They gain access to modern education, state employment, and participation in the post-reform nationalist project. But they lose the ability to read their parents' books, inherit scribal knowledge, or participate in Ottoman scholarly traditions. Constrained: they must learn the new script to succeed in school and career; they cannot refuse without accepting educational exclusion. Their position coordinates across generations (teaching both scripts, bridging the gap) while extracting from them (rendering parental knowledge inaccessible).
narrative_ontology:constraint_stakeholder(rupture_reading, transitional_generation, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rupture_reading, transitional_generation, beneficiary).

% Not an agent but an institutional order: the Ottoman administrative system, the theological school networks (madrasas), the endowment systems (waqfs), the ulema hierarchies — all grounded in Arabic-script transmission and Ottoman-Turkish cultural continuity. These institutions must adapt to survive under the new regime. The reform severs their symbolic and textual foundation, requiring institutional redesign without the authority structures that previously legitimated them. The constraint treats Ottoman institutional order as a victim (payer) rather than as an agent with interests.
narrative_ontology:constraint_stakeholder(rupture_reading, islamic_institutional_continuity, payer,
    institutional, civilizational, constrained, continental).
narrative_ontology:stakeholder_non_agent(rupture_reading, islamic_institutional_continuity).

% Not an agent but a global epistemic formation: the 20th-century narrative that modernization requires rupture with tradition, that script reform signals civilizational progress, that embracing Western-compatible scripts means joining the modern world. This narrative benefits from the Turkish reform as evidence that progress is inevitable and rupture is necessary. The narrative has arbitrage (other nations modernize without script rupture; the narrative could adjust) but chooses persistence because script-rupture-as-modernization is a powerful story for state-builders worldwide.
narrative_ontology:constraint_stakeholder(rupture_reading, modernist_civilization_narrative, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_non_agent(rupture_reading, modernist_civilization_narrative).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Script uniformity solves genuine coordination problems for a centralizing nation-state: enabling standardized education, unifying bureaucratic administration, simplifying legal codification, and allowing mechanized printing and modern technology adoption. These are real problems that script uniformity genuinely addresses.
% TRANSFER_FUNCTION: The reform transfers literacy value (making Ottoman scribal knowledge worthless), access to scholarly tradition (rendering Ottoman texts inaccessible), and cultural continuity (severing generational transmission of Islamic scholarship) FROM the Ottoman literate class and Islamic institutions TO the post-reform state apparatus and Turkish nationalist movement. The transfer is uni-directional extraction, not balanced exchange.
% ABSENT_VOICES: Those who would object but are not in the room: Ottoman scholars exiled or marginalized; Islamic institutions suppressed; diaspora communities; traditional crafts-people whose knowledge is script-specific. These voices are excluded by design — their inclusion would complicate the rupture narrative. The fact that suppression persists (0.82 at t10) suggests that absent voices remain enough of a threat to require continued enforcement.
% DISAPPEARANCE_RATIONALE: If the script reform had not happened, the Ottoman state apparatus would have evolved differently (either toward continued Ottoman-Turkish identity or toward linguistic modernization that preserved script continuity, as in Egypt and Lebanon). Ottoman institutions would have retained textual continuity and scholarly authority. Islamic learning would maintain generational transmission. The new Turkish nation-state would have emerged, but it would have grounded itself in continuity-and-adaptation rather than rupture. Organizational structures, institutional legitimacy, and elite identities all depend on the script change having happened — removing it forces wholesale institutional redesign.
% FOUNDING_PROBLEM: Ottoman Turkish administration faced real coordination challenges: the script's complexity raised literacy barriers; the script's association with Islamic elites excluded secular nationalist modernizers; Arabic-Persian vocabulary created ambiguity in legal codes; non-alphabetic scripts were incompatible with imported printing technologies optimized for Western alphabets. The state also faced a legitimacy challenge: Ottoman identity grounded in Islamic and multi-ethnic diversity conflicted with emerging Turkish nationalism grounded in ethnic and linguistic homogeneity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborating witnesses from OUTSIDE the Turkish state and nationalism: Education researchers documented that literacy rates rose post-reform (independent confirmation of the technical coordination gain). Egypt and Lebanon—Ottoman successor states with different choices—confirm that script continuity does not block modernization (alternative solutions existed). Ottoman chroniclers and travelers pre-reform documented the administration's frustration with script complexity (independent confirmation of the original coordination problem). However, by 1950, the original problem was solved: literacy was rising, printing worked, bureaucracy functioned. The founding problem was dead, yet enforcement remained high (0.82), indicating that the constraint persisted for reasons OTHER than solving the original problem. This is mandatrophy: the constraint outlived its mandate.
narrative_ontology:disappearance_verdict(rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(rupture_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OTTOMAN LITERATE CLASS (SNARE) — Trapped by generational knowledge investment in Arabic-scripted Ottoman Turkish. Script reform renders their literacy worthless, their professional expertise obsolete, and their textual patrimony inaccessible. No exit: the reform is mandatory, total, and enforced through education systems and state administration. They bear maximum extraction — cognitive displacement, professional demotion, and epistemic dispossession.
constraint_indexing:constraint_classification(rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ISLAMIC SCHOLARLY TRADITION (SNARE) — The ulema and quranic scholars cannot maintain scriptural continuity. Arabic script carries both the Qur'an's theological authority and centuries of jurisprudential commentary. Script reform severs this continuity by design — the reading's foundational premise IS the rupture. Trapped by state power; no alternative transmission pathway is permitted. Bears full cost of dispossession from its own textual foundation.
constraint_indexing:constraint_classification(rupture_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: TRANSITIONAL GENERATION (TANGLED ROPE) — Young people forced to learn both scripts; school curriculum coordinates linguistic capability across generations (coordination function) while forcing cognitive displacement and devaluing parents' literacy. Mixed experience: gains access to modern education and state opportunity structures; bears the cost of becoming illiterate in their parents' textual world. Constrained — they have agency within the new system but none to refuse the reform itself.
constraint_indexing:constraint_classification(rupture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POST-REFORM STATE APPARATUS (ROPE) — Primary beneficiary (institutional/arbitrage). Script uniformity solves genuine coordination problems for a centralizing nation-state: simplified literacy instruction, standardized administration, legal codification, and national press all require script uniformity. The state experiences the reform as pure coordination — creating the conditions for the modern bureaucratic apparatus itself. High arbitrage: the state can reverse or modify the reform; it has structural exit capacity. Net beneficiary.
constraint_indexing:constraint_classification(rupture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TURKISH LINGUISTIC NATIONALISM MOVEMENT (ROPE) — Organized actors pursuing linguistic modernization and ethnic homogeneity. Script reform realizes their core agenda: severing Ottoman's Arabic-Persian linguistic traces and constructing a 'pure Turkish' identity. They experience the reform as coordinating a collective identity project. Mobile exit capacity (dissidents can emigrate, refound communities abroad). Net beneficiary — the reform materializes their ideological program.
constraint_indexing:constraint_classification(rupture_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: OTTOMAN/ISLAMIC INSTITUTIONAL CONTINUITY (TANGLED ROPE) — The broader Ottoman administrative and theological tradition experiences simultaneous coordination (the new script enables continued state function) and extraction (the reform's design intent IS to sever continuity with this tradition). Constrained: the institutions must adapt to survive under the new regime. Experiences the reform as enforced modernization that benefits the centralizing state while systematically dismantling the symbolic and textual grounds of Ottoman legitimacy.
constraint_indexing:constraint_classification(rupture_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: MODERNIST IDEOLOGY / CIVILIZATION PROGRESS NARRATIVE (PITON) — The global discourse of modernization treats script reform as evidence of civilizational progress and national maturity. This framing is substantially performative: the theater ratio is high (the reform is marketed as inevitable progress; alternatives are declared unthinkable) while the actual functional gain is modest (many modernization paths don't require script rupture). The narrative persists through institutional inertia — 'modernization requires rupture' is rehearsed as natural law, but evidence shows continuity-and-adaptation pathways in other contexts. Theater sustained because beneficiaries (nation-state builders, modernist intellectuals) collect rents from the inevitability frame.
constraint_indexing:constraint_classification(rupture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From maximum generality, script change could be framed as responding to inherent structural limits of Ottoman written culture: the script's complexity, its association with Islamic elites, its incompatibility with alphabetic printing press technology, its drag on literacy rates. These are presented as facts that 'force' the rupture reading as inevitable. However, this naturalizes what is actually a deliberate cultural-political choice. The falsesummit signature fires here: beneficiaries exist (the state, the nationalist movement), the extractiveness is high (0.78), and the suppression is high (0.82). The mountain framing is a reading-choice, not a discovery of natural limits.
constraint_indexing:constraint_classification(rupture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rupture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rupture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rupture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rupture_reading, TR),
    TR >= 0.70.

:- end_tests(rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): High. Under the rupture reading, the reform is deliberately designed to dispossess the Ottoman literate class and Islamic scholarly tradition. The extraction is not incidental to a technical modernization — it IS the point. The high extractiveness reflects that state benefits from rendering Ottoman literacy worthless (consolidates state control, removes alternative sources of authority) and that the victims (Ottoman scholars, traditional religious institutions) have no exit pathway. The value is tempered slightly from 0.85+ because some genuine coordination function exists (state administration genuinely requires script uniformity) and generational transition enables some adaptation. Suppression (0.82): High. The reform requires sustained institutional enforcement: mandatory education in new script, state prohibition of old-script instruction, bureaucratic prohibition of Arabic-scripted documents in official contexts, informal social pressure against old-script literacy. Suppression remains high even post-enforcement because the institutional infrastructure (schools teaching only new script, archives inaccessible without translation, state documents in new script) perpetuates the barrier. Theater ratio (0.35): Low. This reading has LOW performative content because the rupture intent is directly instantiated in action — enforcement is actual, not theatrical. The reform is not justified through elaborate rhetoric masking a different function; the rhetoric directly expresses the intent. The theater rises slightly over time as modernist ideology narrativizes the reform as inevitable progress, but the core mechanism is enforcement, not performance.
 *
 * PERSPECTIVAL GAP:
 *   The rupture reading produces maximum perspectival divergence because the beneficiary's coordination story and the victim's extraction story are both true structural descriptions. Analytically: the piton perspective shows how modernist discourse naturalizes the reform, rendering the political choice invisible. The mountain reading risks the same error. The false-summit signature fires: beneficiaries exist (state, nationalism), extractiveness is high (0.78), suppression is high (0.82), and the constraint is claimed as natural law of modernization. This naturalness is revelatory — it shows how state-driven extraction gets laundered through universalized narratives about progress.
 *
 * DIRECTIONALITY LOGIC:
 *   The primary directionality divide runs through power and exit options. The Ottoman literate class (powerless/trapped) experiences maximum extraction: d approaches 1.0, and the engine applies the sigmoid f(d) to produce high chi. Their exit is blocked by state enforcement and by the fact that the reform is mandatory and total — no alternative script domain exists. The state apparatus (institutional/arbitrage) experiences minimal extraction (d approaches 0.0): they benefit from the coordination, and they retain exit capacity (could theoretically reverse the reform, though reversing costs are high). Turkish nationalism (organized/mobile) experiences beneficiary status: they designed the reform and can exit if its consequences become unacceptable (emigrate, revert to Arabic, organize diaspora). The transitional generation (moderate/constrained) experiences mixed extraction: they benefit from access to modern education and state opportunity structures but bear the cognitive cost of learning two scripts and the epistemic cost of losing parental textual world. Their directionality is intermediate (d ~0.5–0.6), producing moderate chi. No overrides are needed: the structural derivation from beneficiary/victim + exit options produces appropriate directionality values.
 *
 * MANDATROPHY ANALYSIS:
 *   The rupture reading reveals how mandatrophy manifests in identity and cultural domains. The mandate was 'modernize Turkish administration through script reform.' The original problem: Ottoman Turkish mixed Arabic and Persian vocabulary heavily; Arabic script was associated with Islamic elites; literacy rates were low; printing and mechanized systems were optimized for alphabetic scripts. These are real coordination problems. The rupture reading traces what happened to the mandate: (1) Script reform was enacted (mandate executed). (2) Literacy rates rose, administrative efficiency increased, printing access expanded — manifest coordination benefits. (3) But the reform persisted and intensified BEYOND addressing the original coordination problem — it became an identity project, a rupture for its own sake. (4) By 1950s–1960s, the original mandate (enabling modern administration, raising literacy) was fully satisfied. Yet the reform remained, and enforcement remained high (0.82 suppression at t10), because the constraint had mutated into a identity-boundary maintenance mechanism. The mandate (technical modernization) had outlived its function, but the constraint (rupture from Ottoman identity) persisted because new beneficiaries (nationalist state, elite identity projects) collected rents from continued enforcement. This is textbook mandatrophy: a constraint designed for one function outlives that function and persists because alternative beneficiaries now depend on it. The rupture reading's classification as tangled_rope (rather than temporary scaffold) reflects that no sunset clause was ever written — the state never declared the reform temporary or contingent on reaching literacy targets. It was permanent by design, indicating extractive intent beneath the coordination rhetoric.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rupture_versus_continuity_axis,
    'Is script change fundamentally a rupture reading (deliberate severing of Ottoman/Islamic past) or a continuity reading (adaptive modernization preserving institutional flow)?',
    'Historical analysis of state rhetoric and enforcement: Does official discourse emphasize break (rupture axiom) or evolution (continuity axiom)? Archival evidence from education ministry, legal codes, and official ceremonies. Textual archaeology: Are pre-reform texts systematically destroyed, archived, or translated? Does the state fund or prohibit re-learning Arabic script?',
    'If rupture reading holds: high extractiveness from Ottoman literate populations is structural. If continuity reading prevails: extractiveness is reframed as transition cost in a adaptation process. If competing readings coexist: different constituencies experience the constraint as snare vs tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rupture_versus_continuity_axis, conceptual, 'Whether script change is deliberate rupture or adaptive continuity').

omega_variable(
    necessity_versus_choice,
    'Did script reform respond to genuine structural necessity (literacy barriers, technical incompatibility with printing) or was the necessity constructed through policy choices (education spending allocation, elite ideology)?',
    'Comparative historical analysis: Did other Ottoman successor states or non-Ottoman nations resolve identical technical constraints through script continuity or script change? (Lebanon, Egypt, Iran maintained Arabic script; Turkey switched.) What was the trajectory of literacy rates pre- and post-reform? Was modernization blocked by script or enabled by the reform?',
    'If necessity is genuine: mountain reading may hold (constraint responds to immutable limits). If necessity is constructed: the constraint is snare (pure extraction) or tangled_rope (extraction + coordination) depending on whether genuine coordination function exists alongside extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(necessity_versus_choice, empirical, 'Whether script change responded to structural necessity or constructed necessity').

omega_variable(
    identity_lock_mechanism,
    'Does the rupture reading lock Islamic scholarly identity through internalized identity fusion (identity_locked exit) or through external enforcement (trapped exit)?',
    'Post-suppression ethnographic analysis: Do silenced scholars seek to re-learn Arabic script and transmit it clandestinely (identity_locked — they cannot abandon the scriptural tradition even when enforcement relaxes)? Or do they abandon it once external suppression ends (trapped — external barriers were the binding mechanism)? Generational tracking: Do offspring of Ottoman scholars maintain script loyalty or adopt the new script as primary identity?',
    'If identity_locked: the suppression mechanism persists after state enforcement eases, constraining exit even when material barriers fall. If trapped: suppression lifts when enforcement machinery degrades, releasing agents to reclaim scribal tradition. Identity-lock asymmetry reveals whether the binding is psychological (internalized rupture) or structural (enforced rupture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether scholar displacement is identity-locked or externally trapped').

omega_variable(
    reading_intent_versus_effect,
    'Is the rupture reading''s axiom (script change IS deliberate cultural rupture) describing the state''s actual intent or the observable effect as perceived by dispossessed populations?',
    'Archival analysis of state planning documents, elite correspondence, and ideological manifestos. Compare stated intent (official records) with observable effect (what actually happened to Ottoman literacy, Islamic institutions, textual transmission). Where intent and effect diverge, document whether the divergence was predictable or emergent.',
    'If rupture was intentional and effective: extractive classification holds strongly. If rupture was emergent effect of uncoordinated policy (education choice, printing tech adoption, careers tied to new script): extraction is incidental rather than designed, reframing as tangled_rope. If rupture was intended but ineffective (scripts coexist, tradition persists): classification drops toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_intent_versus_effect, conceptual, 'Intent-effect alignment for the rupture reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rupture_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rupt_theater_t0_pretransition, rupture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rupt_theater_t1_rhetoric_mobilization, rupture_reading, theater_ratio, 1, 0.4).
narrative_ontology:measurement(rupt_theater_t3_enforcement_focus, rupture_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(rupt_theater_t5_stabilized, rupture_reading, theater_ratio, 5, 0.35).
narrative_ontology:measurement(rupt_theater_t10_modernist_narrative, rupture_reading, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(rupt_extractiveness_t0_pretransition, rupture_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rupt_extractiveness_t1_reform_announcement, rupture_reading, base_extractiveness, 1, 0.68).
narrative_ontology:measurement(rupt_extractiveness_t3_mandatory_education, rupture_reading, base_extractiveness, 3, 0.81).
narrative_ontology:measurement(rupt_extractiveness_t5_enforcement_plateau, rupture_reading, base_extractiveness, 5, 0.78).
narrative_ontology:measurement(rupt_extractiveness_t10_generational_effect, rupture_reading, base_extractiveness, 10, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rupt_suppression_t0_pretransition, rupture_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(rupt_suppression_t1_active_enforcement, rupture_reading, suppression_requirement, 1, 0.65).
narrative_ontology:measurement(rupt_suppression_t3_peak_enforcement, rupture_reading, suppression_requirement, 3, 0.88).
narrative_ontology:measurement(rupt_suppression_t5_institutional_lock, rupture_reading, suppression_requirement, 5, 0.82).
narrative_ontology:measurement(rupt_suppression_t10_normalized_enforcement, rupture_reading, suppression_requirement, 10, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rupture_reading, identity_coordination).
narrative_ontology:affects_constraint(rupture_reading, ottoman_institutional_dissolution).
narrative_ontology:affects_constraint(rupture_reading, islamic_scholarly_marginalization).
narrative_ontology:affects_constraint(rupture_reading, turkish_linguistic_nationalism_hegemony).

% DUAL FORMULATION NOTE:
% The script reform operates at the intersection of three constraint families: (1) ADMINISTRATIVE MODERNIZATION (technical necessity, coordination-heavy) — script change solves real literacy and printing problems. (2) IDENTITY RUPTURE (political choice, extraction-heavy) — script change severs Ottoman/Islamic continuity. (3) LINGUISTIC NATIONALISM (ideological project, coordination for the in-group, extraction for the out-group). The rupture reading emphasizes family 2; the continuity reading emphasizes family 1; the modernization reading oscillates between them. Each reading produces different ε values: technical modernization ε~0.30, identity rupture ε~0.78, nationalism ε~0.65. These are not three perspectives on one constraint — they are three distinct constraints sharing a common observable (the 1928 reform) but with different extractiveness profiles. The rupture_reading story instantiates the identity rupture family exclusively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
