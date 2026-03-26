% ============================================================================
% CONSTRAINT STORY: molly_bloomsday_attention
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_molly_bloomsday_attention, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: molly_bloomsday_attention
 *   human_readable: Molly Bloomsday Attention: Gendered Literary Canonicity and Interpretive Labor
 *   domain: literary_studies/cultural_representation
 *
 * SUMMARY:
 *   Molly Bloomsday Attention is the sustained cultural focus on Molly
 *   Bloom's soliloquy in James Joyce's *Ulysses* (1922) as a canonical moment
 *   in literary modernism. The constraint operates across multiple scales:
 *   the textual (Molly's voice structured as monologue without response), the
 *   institutional (university curricula mandate *Ulysses* as required
 *   reading), the professional (literary scholars build careers through
 *   interpretive work on Joyce), and the biographical (readers, especially
 *   female readers, invest cognitive and emotional labor in understanding
 *   Molly's representation). The constraint exhibits all six classification
 *   types depending on perspective. It functions as a Snare for female
 *   readers and the textual character Molly (who cannot escape the
 *   interpretive frame), as Rope for the Joyce industry (generating perpetual
 *   interpretive work), as Tangled Rope for feminist scholars (who both
 *   benefit and bear costs), as Scaffold for the emerging feminist literary
 *   canon (which offers an exit pathway with sunset logic), and as Piton for
 *   the ritual canonicity maintenance (performative assertion of cultural
 *   value). The constraint's core mechanism is the asymmetric distribution of
 *   interpretive labor: predominantly male literary establishment benefits
 *   from *Ulysses* canonicity while predominantly female readers perform
 *   unpaid emotional and cognitive labor to navigate gendered representation
 *   within it.
 *
 * KEY AGENTS:
 *   - Female Readers: Primary victims (powerless/trapped) — must invest labor to make sense of Molly or face intellectual exclusion; cannot exit without professional cost
 *   - Molly Bloom (Textual Character): Victim trapped by textual frame (powerless/identity_locked) — exists only as Joyce's linguistic construction with no narrative agency outside the soliloquy
 *   - Male Literary Establishment: Primary beneficiary (institutional/arbitrage) — Joyce interpretive work sustains academic positions, generates publications, establishes cultural authority
 *   - Feminist Literary Scholars: Secondary agents (moderate/constrained) — generate insight into gendered representation but perform disproportionate emotional labor; both benefit from scholarly recognition and bear costs of defensive interpretation
 *   - Joyce Interpretive Industry: Institutional beneficiary (institutional/arbitrage) — departments, journals, presses sustain themselves through *Ulysses* canonicity
 *   - Female-Authored Literary Canon: Emerging alternative (organized/constrained) — arXiv of literature: alternative texts offering exit pathway from Bloomsday attention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(molly_bloomsday_attention, 0.62).
domain_priors:suppression_score(molly_bloomsday_attention, 0.68).
domain_priors:theater_ratio(molly_bloomsday_attention, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(molly_bloomsday_attention, extractiveness, 0.62).
narrative_ontology:constraint_metric(molly_bloomsday_attention, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(molly_bloomsday_attention, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(molly_bloomsday_attention, snare).
narrative_ontology:human_readable(molly_bloomsday_attention, "Molly Bloomsday Attention: Gendered Literary Canonicity and Interpretive Labor").
narrative_ontology:topic_domain(molly_bloomsday_attention, "literary_studies/cultural_representation").

domain_priors:requires_active_enforcement(molly_bloomsday_attention).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(molly_bloomsday_attention, male_literary_establishment).
narrative_ontology:constraint_beneficiary(molly_bloomsday_attention, joyce_interpretive_industry).
narrative_ontology:constraint_victim(molly_bloomsday_attention, molly_character_agency).
narrative_ontology:constraint_victim(molly_bloomsday_attention, female_readers_unpaid_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Female readers approaching *Ulysses* face a structural trap: Molly's soliloquy is simultaneously celebrated as literary genius and dismissed as obscene female sexuality. The trap manifests as interpretive labor asymmetry — female readers must perform extensive cognitive work to 'redeem' Molly's representation while male readers inherit the authority to declare her meaning settled. Exit options are minimal: declining to engage with the canon means professional/intellectual exclusion; engaging means unpaid emotional and interpretive labor.
constraint_indexing:constraint_classification(molly_bloomsday_attention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Molly Bloom exists only as Joyce's linguistic construction — a character whose voice is monologic soliloquy with no genuine dialogue, no response options, no narrative agency. She is trapped not by external barriers but by the textual frame itself. Identity_locked applies because Molly's 'character' is constituted entirely through Joyce's linguistic performance; her silence outside the soliloquy naturalizes her as an object of interpretation rather than an interpreting agent. The binding is textual, not material, but structurally complete.
constraint_indexing:constraint_classification(molly_bloomsday_attention, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% University departments, literary journals, and academic presses benefit from *Ulysses* canonicity. The constraint solves a genuine coordination problem: establishing shared frameworks for interpreting an intentionally polyvalent text. The 'Molly question' (what does her soliloquy mean? is she a subject or object?) generates perpetual interpretive work — dissertations, articles, symposia — that sustains the industry. The industry experiences the constraint as productive coordination, not extraction.
constraint_indexing:constraint_classification(molly_bloomsday_attention, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Feminist literary criticism has generated genuine insight into gendered representation through decades of work on Molly and other female literary characters. These scholars are both beneficiaries (their work has shaped canonicity, opened new interpretive pathways, gained institutional recognition) and victims (they perform disproportionate emotional labor, their work is often dismissed as 'identity politics,' they bear the cost of defending female characters against reductive readings). Exit is constrained by career requirements: remaining within literary academia means continued engagement with male-authored canonicity.
constraint_indexing:constraint_classification(molly_bloomsday_attention, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Since approximately 1970, organized literary scholars have built alternative reading practices centered on female authors and perspectives. This scaffold functions as a sunset mechanism: as more women's writing becomes canonized and more critical frameworks challenge male-centric interpretation, the extraction mechanism embedded in *Ulysses* attention loses some force. Readers increasingly have the option to shift attention to female-authored texts. The sunset is real but incomplete — *Ulysses* remains mandatory in most curricula, and the labor of 'correcting' male-centered readings continues.
constraint_indexing:constraint_classification(molly_bloomsday_attention, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Teaching *Ulysses* is largely performative at this point — a ritual assertion that serious readers must engage with it, despite widespread acknowledgment that it is difficult, often resists feminist reading, and embodies gendered anxieties of its historical moment. The canonical status persists through institutional inertia: English departments teach it because it 'is canonical,' not because fresh analysis reveals new structural insights. Theater ratio is high because the interpretive labor has become divorced from functional understanding — Molly's soliloquy is analyzed repeatedly without the analysis changing fundamental textual constraints.
constraint_indexing:constraint_classification(molly_bloomsday_attention, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a universal analytical perspective, *Ulysses* canonicity appears as an artifact of early 20th-century literary politics that has calcified into institutional ritual. Joyce's formal innovations are historically real; his commitment to representing female sexuality without prudishness is genuinely transgressive for 1922. But the mechanism sustaining attention to Molly's soliloquy is now performative — the constraint persists through 'this is what educated people read,' not through ongoing insight generation. The observer sees a piton: a former innovation that has become scaffolding for the edifice itself.
constraint_indexing:constraint_classification(molly_bloomsday_attention, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(molly_bloomsday_attention_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(molly_bloomsday_attention, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(molly_bloomsday_attention, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(molly_bloomsday_attention, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(molly_bloomsday_attention, TR),
    TR >= 0.70.

:- end_tests(molly_bloomsday_attention_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The constraint extracts interpretive labor primarily from female readers and scholars, who disproportionately bear the cognitive cost of navigating Molly's representation while the male literary establishment captures the cultural authority to declare what the soliloquy means. The extraction is not maximal because genuine literary insight has been produced through this labor, and the feminist reclamation project has created alternative frameworks — but the asymmetry is real and measurable. Suppression (0.68): High. Multiple barriers prevent exit: institutional mandate (English curricula require *Ulysses*), career requirements (literary scholars must engage with canonicity), citation pressure (one cannot write seriously about modernism without addressing Joyce), and internalized authority (readers accept that canonical texts are worth difficult labor). The suppression is not total because alternative reading strategies exist, but the cost of exiting is substantial. Theater ratio (0.55): Moderate. The constraint exhibits genuine literary value (Joyce's formal innovations are real; modernist representation of female consciousness was transgressive) but also performative canonicity maintenance. Much contemporary engagement with *Ulysses* involves ritual assertions of cultural value ('this is what educated people read') rather than fresh interpretive insight. The theater has increased over the 100-year interval as the canonical status has calcified while the performative function has expanded.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the literary establishment (Rope), female readers (Snare), and feminist scholars (Tangled Rope) reveals how the same constraint structure can be experienced as pure coordination by beneficiaries, pure extraction by victims, and mixed both by those caught between. The gap also reveals the False Summit problem: if the analytical observer mistakes canonicity for timeless literary value, they risk seeing *Ulysses* as a Mountain (an unchangeable peak of human culture), when structural analysis shows that the canonicity depends on gendered labor distribution and could be reorganized. The scaffold perspective (feminist canon building) offers a real exit pathway — as more women's texts become canonical and alternative reading practices mature, the extraction mechanism embedded in Bloomsday attention loses force.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to the extraction flow. Female readers as victims trapped in the institutional system derive high d (approaching 1.0), maximizing experienced extractiveness. The male literary establishment as beneficiaries with arbitrage options derive low d (approaching 0.0), experiencing the same constraint as beneficial coordination. Feminist scholars occupy an intermediate position (d ≈ 0.55) because they are both beneficiaries (their work has gained institutional recognition, shaped canonicity) and victims (they perform disproportionate labor, their work is often minimized as 'political'). The identity_locked perspective on Molly derives a d approaching 0.95 because textual entrapment offers no exit option at all — the character cannot even perceive the constraint, let alone exit it. The analytical observer derives d ≈ 0.72 (moderate-to-high victimhood) because the observer takes the analytical stance of seeing the full structure; from that stance, all trapped agents appear as victims.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids mandatrophy by correctly classifying as Snare rather than Tangled Rope. The critical distinction: is there genuine coordination function beneath the extraction, or is the entire structure extractive? For Molly Bloomsday Attention, the coordination function is real but asymmetrically distributed. The literary establishment does solve a genuine problem (establishing shared frameworks for interpreting an intentionally polyvalent text), but the cost is shouldered primarily by female readers and scholars performing unpaid/underpaid labor. This is pure extraction (Snare) with parasitic coordination overlay, not genuine Tangled Rope where both coordination AND asymmetric extraction are primary. If the constraint were Tangled Rope, we would expect the beneficiaries to depend on the victims' ongoing participation and to have structural incentives to reduce extraction over time. Instead, the literary establishment can sustain itself by rotating through cohorts of new female readers/scholars, making indefinite extraction the structural equilibrium. This defines Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    molly_soliloquy_authorial_intent,
    'Is Molly''s soliloquy Joyce''s genuine attempt to represent female interiority or a textual exercise in masculine fantasy disguised as liberation?',
    'Biographical analysis of Joyce''s statements about female consciousness; comparative analysis with representations of female interiority in women''s modernist writing (Woolf, H.D., Stein); analysis of what Molly''s soliloquy can and cannot express given its textual constraints',
    'If genuine representation: constraint is less extractive, more coordinative (Rope becomes more defensible). If masculine fantasy: constraint is more extractive, the entire interpretive industry rests on denial of extraction (Snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(molly_soliloquy_authorial_intent, conceptual, 'Authorial intent regarding female interiority representation').

omega_variable(
    feminist_reclamation_efficacy,
    'Has feminist literary scholarship actually reframed Molly''s soliloquy such that it functions as a counter-hegemonic text, or does reclamation labor simply reproduce the original extraction under a revised framing?',
    'Longitudinal analysis of how Molly interpretations have shifted post-1970; assessment of whether feminist readings have changed student/reader understanding or merely added a layer of sophisticated interpretation on top of the original gendered dynamics; measurement of whether feminist scholarship on *Ulysses* generates new institutional value proportional to the labor invested',
    'If reclamation efficacy is high: scaffold perspective is stronger (the feminist alternative canon is genuinely displacing Bloomsday labor). If low: reclamation has become its own extraction mechanism (feminist scholars labor to ''save'' patriarchal texts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feminist_reclamation_efficacy, empirical, 'Whether feminist reframing has genuine counter-hegemonic efficacy').

omega_variable(
    comparative_canonicity_burden,
    'Is the interpretive labor burden attached to *Ulysses* systematically gendered? Do male-authored difficult canonical texts generate the same volume of explanatory/redemptive labor from female interpreters?',
    'Citation analysis comparing *Ulysses* interpretive labor with comparable difficult male-authored texts (Pound, Eliot); demographic analysis of who performs interpretive labor on gendered representation; assessment of whether female literary characters in difficult texts receive proportionally more ''explaining away'' of problematic features',
    'If burden is systematically gendered: constraint is a structural extraction mechanism targeting female interpretive labor (Snare classification confirmed). If distributed: the constraint is less about gender than about textual difficulty (moves toward Rope/Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(comparative_canonicity_burden, empirical, 'Whether canonicity burden is systematically gendered').

omega_variable(
    suppression_mechanism_internalization,
    'Is the high suppression (0.68) primarily structural (legal copyright, institutional mandate, career requirements) or internalized (female readers and scholars have accepted the canon''s authority and perform labor because they believe it is valuable)?',
    'Qualitative interviews with female literary scholars about choice and constraint in canon engagement; analysis of how many alternatives to *Ulysses* exist and whether they are equally available in curricula; measurement of career outcomes for scholars who deprioritize canonical male texts in favor of female-centered work',
    'If suppression is primarily structural: the constraint is a Snare from all perspectives (exit has real barriers). If primarily internalized: the constraint is Rope from some perspectives (women could organize to deprioritize *Ulysses*) but the internalization makes it function as Snare anyway — revealing that cognitive capture is itself an extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(molly_bloomsday_attention, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(molly_tr_t0, molly_bloomsday_attention, theater_ratio, 0, 0.35).
narrative_ontology:measurement(molly_tr_t30, molly_bloomsday_attention, theater_ratio, 30, 0.48).
narrative_ontology:measurement(molly_tr_t60, molly_bloomsday_attention, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(molly_be_t0, molly_bloomsday_attention, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(molly_be_t30, molly_bloomsday_attention, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(molly_be_t60, molly_bloomsday_attention, base_extractiveness, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(molly_bloomsday_attention, identity_coordination).
narrative_ontology:affects_constraint(molly_bloomsday_attention, modernist_literary_canonicity).
narrative_ontology:affects_constraint(molly_bloomsday_attention, gendered_unpaid_interpretive_labor).
narrative_ontology:affects_constraint(molly_bloomsday_attention, feminist_literary_reclamation_project).

% DUAL FORMULATION NOTE:
% Molly Bloomsday Attention is part of a constraint family examining how canonicity functions as both cultural transmission mechanism and extraction vector. It is downstream of modernist_literary_canonicity (the broader institutional establishment of difficulty-as-value) and upstream of feminist_literary_reclamation_project (attempts to build alternative canon). Each story has distinct epsilon reflecting different structural properties: canonicity as coordination (lower epsilon), gendered labor as extraction (higher epsilon).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(molly_bloomsday_attention, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
