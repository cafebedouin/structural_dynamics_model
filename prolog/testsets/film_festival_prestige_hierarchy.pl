% ============================================================================
% CONSTRAINT STORY: film_festival_prestige_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_film_festival_prestige_hierarchy, []).

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
 *   constraint_id: film_festival_prestige_hierarchy
 *   human_readable: Film Festival Prestige Hierarchy
 *   domain: cultural/entertainment/institutional
 *
 * SUMMARY:
 *   The film festival prestige hierarchy functions as a gating mechanism that
 *   coordinates genuine artistic curation while simultaneously extracting
 *   career rents from filmmakers dependent on its validation. The constraint
 *   exhibits classic tangled rope structure: tier-one festivals (Cannes,
 *   Venice, Berlin) provide real coordination value through curation and
 *   discovery, yet the hierarchy's tiered structure suppresses emerging
 *   filmmakers and independent producers who lack conventional credentials.
 *   The hierarchy is enforced through theatrical ritual (mysterious selection
 *   committees, prestige rhetoric) and structural suppression (submission
 *   fees, gatekeeping asymmetries, publication bias in rejections). Over the
 *   past 30 years, theater ratio has risen from 0.52 to 0.68 as the curation
 *   authority has become increasingly performative: festival selections are
 *   justified through mystified taste rhetoric while actual selection
 *   reflects industry gatekeeping and filmmaker networks. Simultaneously,
 *   algorithmic discovery and streaming platforms have created alternative
 *   prestige mechanisms that function with lower theatrical overhead. The
 *   constraint operates at global scope but with particularly high
 *   suppression in non-canonical cinema traditions (non-English films,
 *   radical cinema, non-narrative forms) and for filmmakers without
 *   institutional backing.
 *
 * KEY AGENTS:
 *   - Tier-One Festivals (Cannes, Venice, Berlin): Institutional beneficiaries (institutional/arbitrage) — capture curation authority and prestige brand maintenance; experience constraint as coordination
 *   - Established Filmmakers: Secondary beneficiaries (powerful/arbitrage) — already have conventional credentials; festivals provide network amplification and distribution deals without gatekeeping suppression
 *   - Festival Gatekeepers (curators, programmers): Institutional agents (institutional/arbitrage) — maintain authority through hierarchy; beneficiaries of mystified selection rhetoric that justifies their judgment
 *   - Emerging Filmmakers: Primary victims (powerless/trapped) — career progression entirely dependent on festival validation; face resource barriers and insider nepotism; cannot exit without abandoning conventional legitimacy
 *   - Independent Producers: Secondary victims (moderate/constrained) — can theoretically bypass festivals but face prestige loss and distribution disadvantages; high cost to exit conventional pathway
 *   - Non-Canonical Cinema Traditions: Tertiary victims (powerless/identity_locked) — suppressed not just by resource barriers but by curation criteria that treat Western institutional cinema as canonical; internalized belief in festival legitimacy as legitimate artistic judgment prevents recognition of gatekeeping
 *   - Decentralized Film Coalition: Organized agents (organized/constrained) — building alternative prestige mechanisms (film co-ops, algorithmic curation, regional festivals); creating visible exit pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(film_festival_prestige_hierarchy, 0.58).
domain_priors:suppression_score(film_festival_prestige_hierarchy, 0.65).
domain_priors:theater_ratio(film_festival_prestige_hierarchy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(film_festival_prestige_hierarchy, extractiveness, 0.58).
narrative_ontology:constraint_metric(film_festival_prestige_hierarchy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(film_festival_prestige_hierarchy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(film_festival_prestige_hierarchy, tangled_rope).
narrative_ontology:human_readable(film_festival_prestige_hierarchy, "Film Festival Prestige Hierarchy").
narrative_ontology:topic_domain(film_festival_prestige_hierarchy, "cultural/entertainment/institutional").

domain_priors:requires_active_enforcement(film_festival_prestige_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(film_festival_prestige_hierarchy, tier_one_festivals).
narrative_ontology:constraint_beneficiary(film_festival_prestige_hierarchy, established_filmmakers).
narrative_ontology:constraint_beneficiary(film_festival_prestige_hierarchy, festival_gatekeepers).
narrative_ontology:constraint_victim(film_festival_prestige_hierarchy, emerging_filmmakers).
narrative_ontology:constraint_victim(film_festival_prestige_hierarchy, independent_producers).
narrative_ontology:constraint_victim(film_festival_prestige_hierarchy, non_canonical_cinema).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING FILMMAKER (SNARE) — Career progression requires festival validation at recognized tiers. Cannot exit the prestige system without abandoning conventional career pathways. Suppressed by publication bias (rejection rarely explained), resource barriers (submission fees, travel costs), and nepotistic selection (established directors' films get preferential slots). The trap is structural: the career reward mechanism depends on the festival validation mechanism.
constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER FILMMAKER (TANGLED ROPE) — Experiences both coordination and extraction. Festival selections genuinely enable networking, distribution deals, and audience access — coordination benefits are real. But the hierarchy extracts through winnowing: only a small percentage receive premium slots, and tier placement determines career trajectory. Exit is theoretically possible (direct-to-streaming, film co-ops) but costly — requires abandoning conventional legitimacy pathways and accepting reduced prestige.
constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TIER-ONE FESTIVAL (ROPE) — Benefits from prestige hierarchy through curation authority and brand maintenance. Experiences the constraint as coordination: selecting from high-quality submissions and creating discoverable venues for quality cinema. Net beneficiary with low coercion overhead — the festival maintains prestige through genuine curation, not through suppression of alternatives. Can arbitrage their selection authority across funding, partnerships, and institutional support.
constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY CURATOR ESTABLISHMENT (PITON) — The prestige hierarchy is maintained through institutional inertia and theatrical ritual. Canonicity (Cannes, Venice, Berlin) persists not because the festivals uniquely identify quality cinema but because 50+ years of institutional endorsement created a self-fulfilling prophecy. The theater is high: festival selections are justified through mystified curation rhetoric while actual selection reflects industry gatekeeping. Film scholarship and industry practice treat the hierarchy as natural and inevitable, despite mounting evidence that distributed discovery (social media, algorithmic curation, regional festivals) identifies quality cinema at comparable or better rates. The constraint is degraded — it functionally sorts films but no longer performs its original role (discovering new talent, maintaining cinema's artistic boundary).
constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DECENTRALIZED FILM COALITION (SCAFFOLD) — Organized alternative pathways (film co-ops, regional festivals, online platforms, artist-led curations) are building parallel prestige mechanisms that bypass the tier-one hierarchy. These mechanisms have genuine sunset logic: as algorithmic discovery, social media, and distributed platforms mature, the traditional festival hierarchy's gatekeeping function becomes increasingly redundant. Exit path is visible: filmmakers can build audiences and distribute directly, using decentralized validation. The constraint persists but is structurally temporary — its suppression mechanisms (fee structures, gatekeeping rhetoric) are declining in effectiveness as alternatives mature.
constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some prestige hierarchy is inherent to cultural institutions: every artistic tradition must have mechanisms for canonical status and quality differentiation. No way to have 'cinema' without some films being more recognized than others. This perspective naturalizes the constraint as an immutable feature of how cultural value gets distributed. However, the structural data contradicts the mountain classification: the specific tier-one festival hierarchy is contingent institutional arrangement (Cannes gained primacy through WWII positioning, not through inherent quality superiority). The 'immutability' claim is false naturalization of a particular historical instantiation of prestige mechanisms.
constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(film_festival_prestige_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(film_festival_prestige_hierarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(film_festival_prestige_hierarchy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(film_festival_prestige_hierarchy, TR),
    TR >= 0.70.

:- end_tests(film_festival_prestige_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The prestige hierarchy extracts through career rent concentration: a small percentage of submissions receive premium slots and career acceleration, while the vast majority are winnowed without explanation. The extraction is not maximal because the coordination function is genuine — tier-one festivals do identify quality cinema at rates better than random selection, and the curation has real artistic value. But the extraction component is substantial because the hierarchy is artificially concentrated (four festivals dominate global cinema prestige) and because alternative discovery mechanisms are suppressed through prestige rhetoric rather than enabled. Suppression (0.65): High. Emerging filmmakers face multiple suppression mechanisms: submission fees ($75-150 per festival × hundreds of submissions), resource barriers (travel, residency requirements), publication bias (rejections unexplained), and insider nepotism (established filmmakers' films fast-tracked through selection committees). These barriers are not insurmountable individually but collectively create high suppression. Theater ratio (0.68): High and rising. Festival selections are justified through mystified curation rhetoric ('the committee's artistic vision,' 'international recognition of talent') while actual selection reflects industry gatekeeping, filmmaker networks, and prestige accumulation. The theatrical component has increased as streaming platforms (Netflix, Amazon) have begun producing 'festival selections,' inverting the discovery mechanism — instead of festivals discovering films, funded platforms now use festival selections for legitimacy. The rise in theater ratio from 0.52 to 0.68 reflects increasing performative content: the curation authority is maintained rhetorically while functional discovery increasingly happens elsewhere.
 *
 * PERSPECTIVAL GAP:
 *   The prestige hierarchy demonstrates extreme perspectival divergence. Tier-one festivals see coordination (Rope) — they genuinely curate quality cinema and provide discovery function. Established filmmakers see prestige amplification (Rope) — festivals enhance their already-strong position. Emerging filmmakers see pure extraction (Snare) — the hierarchy depends on their suppression for prestige differentiation and career winnowing. Mid-tier filmmakers see mixed coordination and extraction (Tangled Rope) — the festivals enable networking but suppress mobility through prestige tiering. The decentralized coalition sees a temporary mechanism with visible sunset (Scaffold) — algorithmic discovery and streaming platforms are building alternatives. The legacy establishment sees degraded ritual (Piton) — they maintain the hierarchy through institutional inertia despite mounting evidence of functional decline. The analytical observer risks naturalizing the hierarchy as an immutable feature of culture (Mountain) — but the structural data reveals that specific tier-one dominance is contingent (Cannes' primacy derived from WWII positioning, not from superior curation) and that alternative prestige mechanisms function equivalently.
 *
 * DIRECTIONALITY LOGIC:
 *   The prestige hierarchy's directionality is determined by structural position relative to the gating mechanism. Beneficiaries with strong exit options (established filmmakers, tier-one festivals) experience the constraint as largely beneficial coordination — they can use festival prestige strategically without depending on it. Victims with trapped options (emerging filmmakers) experience maximum extraction — their entire career viability depends on acceptance into the hierarchy, and rejection carries no explanation. Victims with constrained options (mid-tier filmmakers, independent producers) experience mixed extraction and coordination — they benefit from networking and distribution but face prestige penalties for alternative pathways. The identity-locked component is specific: filmmakers from non-canonical traditions often cannot perceive the prestige hierarchy as contingent even when they intellectually recognize it as gatekeeping, because their identity as filmmakers has been constituted through the prestige system's judgments. Exit would require not just changing distribution mechanisms but reimagining what legitimizes cinema as art.
 *
 * MANDATROPHY ANALYSIS:
 *   The prestige hierarchy resolves mandatrophy by showing that all six types are legitimate readings of the same constraint from different structural positions. The tangled rope classification at the analytical level (moderate power, constrained exit, biographical time) captures the hybrid nature: genuine curation coordination exists alongside genuine suppressive extraction. The mountain classification from the civilizational perspective is a false summit — the claim that 'prestige hierarchies are inherent to culture' naturalizes what is actually a contingent institutional arrangement (Cannes' prominence derives from specific historical positioning, not from superior artistic judgment). The piton classification correctly identifies that the hierarchy is increasingly performative: its functional role (identifying quality cinema) is being performed by algorithms and platforms, yet it persists through institutional inertia and prestige rhetoric. The scaffold classification shows that the constraint is structurally temporary — decentralized discovery is creating genuine exit paths. No single type fully captures the constraint; the presheaf over the observation site contains all six types as legitimate perspectives. The mandata resolution is that the hierarchy's type-ambiguity reveals its character as a contestable institutional arrangement, not a natural or inevitable feature of cinema.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_discovery_sufficiency,
    'Do algorithmic curation systems and social media discovery identify cinema quality and importance at rates comparable to or better than traditional festivals?',
    'Longitudinal analysis of films identified by algorithmic platforms vs festival selections; tracking of audience engagement, critical reception, and industry impact; comparison of discovery rates for diverse cinema traditions across both systems',
    'If comparable/better: scaffold sunset is real — decentralized discovery is functionally replacing festival gatekeeping. If inferior: festival hierarchy has genuine curation function, and suppression of alternatives is coordination cost rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_discovery_sufficiency, empirical, 'Whether algorithmic discovery provides equivalent prestige/quality identification').

omega_variable(
    canonical_cinema_definition_instability,
    'Is the tier-one festival hierarchy''s definition of ''quality cinema'' stable over time, or does it drift to maintain gatekeeping authority?',
    'Historical analysis of selection criteria changes; comparison of films rejected by tier-one festivals vs those selected by second-tier festivals over 20-year windows; tracking of retrospective reassessments (films dismissed at time of release but canonized later)',
    'If definition drifts: piton classification confirmed — hierarchy is performative ritual degrading over time. If stable: mountain or rope classification more supported — hierarchy genuinely identifies quality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(canonical_cinema_definition_instability, empirical, 'Stability of festival curation criteria over time').

omega_variable(
    suppression_mechanism_efficacy,
    'How much of the emerging filmmaker''s suppression is structural (resource barriers, publication bias) vs cognitive (internalized belief in festival legitimacy)?',
    'Post-exit trajectory analysis: filmmakers who bypass festivals through streaming/direct distribution — do they report internalized suppression persisting after external barriers removed? Surveys of decision-making about festival submissions vs alternative distribution.',
    'If substantially internalized: constraint''s effective suppression is higher than metrics suggest — the cognitive capture is part of the extraction mechanism. If mostly structural: removing barriers (fee waivers, algorithmic curation) would enable rapid exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_efficacy, empirical, 'Proportion of suppression that is structural vs internalized').

omega_variable(
    regional_festival_independence,
    'Do second-tier and regional festivals function as genuine alternatives or as de facto satellites of the tier-one hierarchy?',
    'Network analysis of film circulation: do selected films flow upward from regional to tier-one festivals, or do tier-one selections exclude regional discoveries? Measurement of institutional independence in curation authority.',
    'If satellites: no genuine exit path exists (scaffold fails). If independent: emerging filmmakers can build careers through alternative hierarchies (scaffold confirmed, exit options improved).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regional_festival_independence, empirical, 'Whether regional festivals function as independent alternatives or satellites').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(film_festival_prestige_hierarchy, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffph_tr_t0, film_festival_prestige_hierarchy, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ffph_tr_t15, film_festival_prestige_hierarchy, theater_ratio, 15, 0.64).
narrative_ontology:measurement(ffph_tr_t30, film_festival_prestige_hierarchy, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(ffph_be_t0, film_festival_prestige_hierarchy, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ffph_be_t15, film_festival_prestige_hierarchy, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(ffph_be_t30, film_festival_prestige_hierarchy, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(film_festival_prestige_hierarchy, identity_coordination).
narrative_ontology:affects_constraint(film_festival_prestige_hierarchy, film_criticism_gatekeeping).
narrative_ontology:affects_constraint(film_festival_prestige_hierarchy, cinema_canon_definition).
narrative_ontology:affects_constraint(film_festival_prestige_hierarchy, independent_film_distribution).

% DUAL FORMULATION NOTE:
% The prestige hierarchy decomposes into at least three structurally distinct constraints: (1) festival curation as information standard (low ε, coordination type information_standard), (2) prestige tiering as career rent extraction (higher ε, tangled_rope), and (3) theatrical legitimation through selection mystification (high theater_ratio, piton). This story focuses on the tangled rope core; related stories should address curation mechanics and legitimation rhetoric separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(film_festival_prestige_hierarchy, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
