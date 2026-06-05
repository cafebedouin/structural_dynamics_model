% ============================================================================
% CONSTRAINT STORY: remonstrance_authority__crown_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_remonstrance_authority__crown_reading, []).

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
 *   constraint_id: remonstrance_authority__crown_reading
 *   human_readable: Remonstrance Authority as Crown-Delegitimizing Veto (Crown Reading)
 *   domain: constitutional_history/political_economy/legal_authority
 *
 * SUMMARY:
 *   The remonstrance authority constraint, from the Crown reading, models the
 *   structural dynamics of magistrate obstruction of royal fiscal policy in
 *   ancien régime France and Spain (16th-18th centuries). Parlements and
 *   provincial estates wielded remonstrance rights — formal objections to
 *   edicts with enforced delay periods — ostensibly to protect subjects from
 *   arbitrary taxation. From the Crown's structural perspective, remonstrance
 *   functioned as an illegitimate veto mechanism: magistrates systematized
 *   its use to block necessary fiscal reforms (particularly those targeting
 *   noble exemptions), extract concessions (jurisdictional autonomy,
 *   hereditary office protections, tax immunities), and impose theater-laden
 *   delays that weakened crown authority. The constraint exhibits high
 *   extractiveness (0.68) and suppression (0.72) because magistrates face no
 *   material cost to remonstration — the institutional design coerces
 *   compliance through legitimacy threats ('remonstrance is the people's
 *   voice', 'defying magistrates provokes popular uprising') rather than
 *   physical force. Theater (0.58) reflects the rising performative content
 *   of remonstrance debates: as the 17th-18th centuries progressed,
 *   remonstrance letters became increasingly elaborate juridical
 *   pronouncements with minimal substantive engagement, serving to delay
 *   edicts while building plausible legitimacy narratives. The interval (0-40
 *   years) represents the escalation phase from early remonstrance usage
 *   (moderate extraction) to mature institutionalization (high extraction and
 *   suppression). The measurements show extraction accumulation:
 *   base_extractiveness rises 42→68% as magistrates learn to weaponize
 *   remonstrance; suppression rises 52→72% as legitimate-defiance framing
 *   becomes more sophisticated; theater rises 48→58% as remonstrance
 *   documents become more formulaic.
 *
 * KEY AGENTS:
 *   - Crown/Monarchy (institutional/trapped): Primary victim — cannot exit remonstrance obligations; faces systematic fiscal obstruction and legitimacy erosion
 *   - Magistrate Class (institutional/arbitrage): Primary beneficiary — uses remonstrance to extract tax exemptions and jurisdictional autonomy while framing obstruction as constitutional principle
 *   - Subject Population (moderate/constrained): Secondary victim — nominal beneficiary of remonstrance's check on tyranny, but faces fiscal instability and delayed public goods when magistrates block necessary taxation
 *   - Monarchical Revenue System (institutional/trapped): Structural victim — remonstrance delays compound into fiscal cascade, weakening crown capacity and enforcement credibility
 *   - Constitutional Reform Movement (organized/mobile): Organized actor seeing remonstrance as temporary/soluble through constitutional settlement; sees sunset mechanism in written constitutionalism
 *   - Analytical Observer (analytical/analytical): Risks naturalizing contingent institutional arrangement as immutable structural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(remonstrance_authority__crown_reading, 0.68).
domain_priors:suppression_score(remonstrance_authority__crown_reading, 0.72).
domain_priors:theater_ratio(remonstrance_authority__crown_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(remonstrance_authority__crown_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(remonstrance_authority__crown_reading, snare).
narrative_ontology:human_readable(remonstrance_authority__crown_reading, "Remonstrance Authority as Crown-Delegitimizing Veto (Crown Reading)").
narrative_ontology:topic_domain(remonstrance_authority__crown_reading, "constitutional_history/political_economy/legal_authority").

domain_priors:requires_active_enforcement(remonstrance_authority__crown_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(remonstrance_authority__crown_reading, '13b14c02-786d-40bd-8ecd-97477b011280').
narrative_ontology:cs_kernel_codification('13b14c02-786d-40bd-8ecd-97477b011280', formalized).
narrative_ontology:cs_authority_grounding('13b14c02-786d-40bd-8ecd-97477b011280', lineage).
narrative_ontology:cs_interpretation_layer_present('13b14c02-786d-40bd-8ecd-97477b011280').
narrative_ontology:cs_reading_relation('13b14c02-786d-40bd-8ecd-97477b011280', remonstrance_authority__magistrate_reading, coexists_with).
narrative_ontology:cs_axiom('13b14c02-786d-40bd-8ecd-97477b011280', foundational, royal_authority_emanates_from_crown_prerogative).
narrative_ontology:cs_axiom_status(royal_authority_emanates_from_crown_prerogative, holdable).
narrative_ontology:cs_axiom_grounding('13b14c02-786d-40bd-8ecd-97477b011280', royal_authority_emanates_from_crown_prerogative, deontological).
narrative_ontology:cs_axiom('13b14c02-786d-40bd-8ecd-97477b011280', foundational, remonstrance_abuse_obstructs_governance).
narrative_ontology:cs_axiom_status(remonstrance_abuse_obstructs_governance, holdable).
narrative_ontology:cs_axiom_grounding('13b14c02-786d-40bd-8ecd-97477b011280', remonstrance_abuse_obstructs_governance, empirically_contingent).
narrative_ontology:cs_reference_frame('13b14c02-786d-40bd-8ecd-97477b011280', royal_fiscal_supremacy_with_consultation).
narrative_ontology:cs_drift_state('13b14c02-786d-40bd-8ecd-97477b011280', eighteenth_century_escalation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('13b14c02-786d-40bd-8ecd-97477b011280', '').
narrative_ontology:cs_kernel_id(remonstrance_authority__crown_reading, remonstrance_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, noble_aristocratic_class).
narrative_ontology:constraint_beneficiary(remonstrance_authority__crown_reading, provincial_magistrates).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, royal_fiscal_authority).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, monarchical_legitimacy).
narrative_ontology:constraint_victim(remonstrance_authority__crown_reading, crown_revenue_collection).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROWN FISCAL AUTHORITY (SNARE) — The monarchy cannot exit remonstrance obligations; faces systematic obstruction of legitimate tax collection and regulatory authority. Magistrates weaponize remonstrance rights to block fiscal policies without submitting alternative revenue proposals. Suppression via mandatory delays, public denunciations, and institutional theater. No alternative enforcement pathway available once remonstrance is invoked.
constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MAGISTRATE CLASS (ROPE) — Primary beneficiary. Remonstrance provides coordination mechanism for legitimate grievances (preventing arbitrary taxation) AND asymmetric extraction lever (blocking unpopular but necessary fiscal reforms). Magistrates experience the constraint as pure coordination—a check on tyranny—while systematically using it to extract exemptions from taxes they dislike. Exit options abundant: can arbitrage exemptions across jurisdictions, negotiate settlements, or withdraw cooperation selectively.
constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: SUBJECT POPULATION (TANGLED ROPE) — Mixed experience. Remonstrance nominally protects against arbitrary royal taxation, providing genuine coordination benefit. But magistrates use remonstrance to block taxes that fund crown services (military, infrastructure, judicial administration). Result: subjects face prolonged fiscal instability, delayed public works, and magnified tax burdens when compliance finally occurs. Constrained exit: cannot organize alternative verification, cannot dissolve magistrate authority.
constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MONARCHICAL REVENUE SYSTEM (SNARE) — Structurally trapped. Remonstrance delays compound exponentially; each deferred tax creates fiscal cascade that weakens crown capacity to enforce future edicts. Magistrates extract concessions (tax exemptions, jurisdictional autonomy, hereditary office protections) in exchange for compliance. Crown cannot exit; cannot dismantle remonstrance without precipitating institutional crisis. Suppression mechanism: compliance is coerced through legitimacy threats (charges of tyranny, popular uprising rhetoric).
constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: CONSTITUTIONAL REFORM (SCAFFOLD) — Organized agents (emerging legal theorists, parliamentary factions, enlightenment thinkers) recognize remonstrance as a temporary institutional coordination mechanism requiring sunset. See the constraint as soluble through codified constitutionalism: written constraint on royal authority + formalized legislative process to replace ad-hoc remonstrance. Exit mechanism: constitutional settlement that absorbs remonstrance function into legitimate parliamentary procedure. Sunset trajectory: as written constitutions mature, remonstrance loses its extraction leverage.
constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, remonstrance appears as an immutable structural feature of any authority system: subordinate actors will always find mechanisms to check superior authority, and legitimacy always requires subordinate consent. Remonstrance is the inevitable emergent property of asymmetric power attempting to stabilize without consent. However, the structural data reveals this as false summit: remonstrance is a contingent institutional artifact dependent on specific legal framings and enforcement practices, not a law of nature.
constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(remonstrance_authority__crown_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(remonstrance_authority__crown_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(remonstrance_authority__crown_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(remonstrance_authority__crown_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Magistrates systematically use remonstrance to block fiscal policies that would reduce their tax exemptions, extract concessions in exchange for compliance, and accumulate jurisdictional autonomy. The extraction is not total (0.72 would indicate near-complete target immobility) because the crown retains ultimate enforcement capacity (can override remonstrance, though at legitimacy cost) and magistrates cannot extract from the crown indefinitely without weakening crown capacity to enforce remonstrance-coerced settlements. The rising trajectory (42→68%) reflects learning effects: early remonstrance usage was more sporadic and less systematized; as magistrates professionalized their juridical objections and built legitimacy narrative infrastructure, extraction became reliable and enforceable. Suppression (0.72): High. Coercion operates primarily through legitimacy threats, not physical force: magistrates can tie edicts indefinitely through remonstrance ritual without direct military confrontation. The crown faces a choice between legitimacy erosion (overriding magistrates) and fiscal paralysis (accepting delays). Theater (0.58): Moderate-high. Remonstrance letters evolved from substantive objections to increasingly formulaic juridical performances; by the 18th century, the theater of elaborate legal argumentation dominated the actual negotiation. However, theater is not overwhelming (0.70+) because remonstrance outcomes were materially consequential: magistrates did extract real concessions (exemptions, autonomy, hereditary protections), not merely performative victories. The theater tracks the escalation of legitimacy framing without completely displacing material interests.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is fundamental and illuminates the committer-axis disagreement. From the crown's perspective (powerless/trapped): remonstrance is pure extraction (snare) — magistrates obstruct necessary policy with no substantive alternative proposals, coerce compliance through legitimacy threats, and accumulate privileges. From the magistrate's perspective (institutional/arbitrage): remonstrance is pure coordination (rope) — it solves the collective action problem of preventing arbitrary royal taxation and channels legitimate grievances. From the subject's perspective (moderate/constrained): remonstrance is mixed (tangled_rope) — it provides nominal protection against tyranny but also enables magistrate extraction and fiscal dysfunction. From the constitutional reform perspective (organized/mobile): remonstrance is temporary and soluble (scaffold) — seen as a transitional coordination mechanism being replaced by constitutional law. The analytical observer's natural-law framing (mountain) reveals the false summit: the universality of subordinate authority-checking mechanisms is real, but remonstrance is a contingent institutional form, not an inevitable law. The crown reading's snare classification is perspectival to crown interests — it depends on treating magistrate authority as illegitimate and extraction as obstructive. The magistrate reading (sibling constraint) reverses this: magistrate legitimacy is foundational, crown fiscal authority is the illegitimate extraction, and remonstrance is the legitimate check.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown's directionality derives from victim status + trapped exit: the crown cannot walk away from remonstrance obligations without institutional collapse. This produces high d (0.85+) and strong f(d), amplifying experienced extraction. Magistrates' directionality derives from beneficiary status + arbitrage exit: they can selectively invoke remonstrance, negotiate settlements, or withdraw cooperation. This produces low d (0.15-0.25) and negative f(d), dampening their experienced extraction. The structural disagreement is not about directionality calculation but about whose extraction counts as 'extraction': the crown framing treats magistrate obstruction as extraction from the crown; the magistrate framing treats fiscal exemptions + autonomy as the crown's extraction from magistrates. Each reading is internally self-consistent in its directionality assignments; the readings conflict at the level of which agent is the target.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-SPECIFIC MANDATROPHY: This crown reading resolves mandatrophy by accepting the snare classification as perspectival to crown structural position — the constraint genuinely extracts from the crown and genuinely coordinates magistrate interests. The resolution is not 'snare is correct and rope is wrong' but rather 'which perspective are you measuring from?' The crown reading's mandate is to clarify the crown's structural experience: remonstrance functions as extraction mechanism for the crown, legitimate appearance notwithstanding. The magistrate reading (sibling) will have equal analytical validity from magistrate perspective. The full mandatrophy resolution requires both readings in conversation: the constraint is simultaneously legitimate (magistrate) check and illegitimate (crown) veto, depending on which institutional authority's legitimacy is treated as foundational. The engine's role is to display both readings and show how single-perspective analysis naturalizes one party's legitimacy claim while obscuring the other's. The crown reading accomplishes this by showing high extraction from crown perspective; the magistrate reading will accomplish it from the inverse perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remonstrance_vs_constitutional_veto,
    'Does remonstrance represent a legitimate proto-constitutional check on monarchy, or an illegitimate minoritarian veto protecting aristocratic tax privileges?',
    'Empirical analysis of remonstrance usage patterns: percentage of remonstrances targeting fiscal reform vs. protecting specific aristocratic exemptions; correlation between remonstrance and contemporaneous tax revenue loss; comparison to alternative mechanisms (peasant uprising, military defection, administrative failure) in constraining royal authority',
    'If legitimate check: constraint reclassifies toward Tangled Rope from crown perspective (coordination benefit is real, not theater). If illegitimate veto: suppression gates remain high, snare classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remonstrance_vs_constitutional_veto, empirical, 'Whether remonstrance is constitutional check or aristocratic tax veto').

omega_variable(
    magistrate_coordination_necessity,
    'Would the same coordination function (preventing arbitrary royal taxation) be achievable through alternative mechanisms (written law, permanent legislative body, judicial review) without remonstrance''s extraction leverage?',
    'Comparative institutional analysis: when alternative mechanisms (e.g., English Bill of Rights, French pre-revolutionary parlements, Spanish fuero systems) were implemented, did they reduce both arbitrary taxation AND magistrate extraction, compared to remonstrance-dependent systems?',
    'If alternatives sufficient: remonstrance is pure extraction mechanism masquerading as necessary coordination (snare confirmed from all perspectives). If alternatives insufficient: remonstrance provides unique coordination value despite extraction (snare becomes tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(magistrate_coordination_necessity, empirical, 'Whether alternative mechanisms can replace remonstrance''s coordination function').

omega_variable(
    crown_reading_axiom_contestation,
    'Is the Crown''s framing of remonstrance as ''illegitimate minoritarian veto'' a genuine structural claim about magistrate incentives, or itself a legitimacy narrative deployed to extract compliance from magistrates?',
    'Historical analysis of crown edicts and proclamations: do crown documents characterize remonstrance as ''veto'' or as ''consultation mechanism''? Track shifts in framing alongside changes in crown fiscal demands and magistrate resistance. Compare crown public rhetoric to private correspondence.',
    'If crown framing is genuine structural claim: the axis of disagreement is empirical (whether remonstrance actually blocks necessary fiscal reform). If crown framing is itself legitimacy theater: the crown is itself deploying extraction mechanism, making this constraint structurally symmetric (both crown and magistrates extracting via authority claims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crown_reading_axiom_contestation, empirical, 'Whether crown framing of remonstrance is structural or rhetorical').

omega_variable(
    reading_kernel_underdetermination,
    'Does the same historical remonstrance practice instantiate the crown_reading (veto protecting privileges) or the magistrate_reading (legitimate check on tyranny), or does this decompose into two distinct institutional arrangements that both get labeled ''remonstrance''?',
    'Archival analysis of specific remonstrance acts: classify each by whether magistrate motivation is privilege-protection (exemptions, autonomy, heredity), legitimate grievance redress (preventing arbitrary rule, ensuring fair assessment), or both. Build empirical distribution of motivational types. If clear clustering: two distinct constraints. If continuous distribution: one constraint with perspectival disagreement.',
    'If two constraints: create separate story for magistrate_reading with different ε and beneficiary/victim alignment; link via network.affects_constraints. If one constraint with perspectival disagreement: the omegas correctly resolve via empirical measurement of actual remonstrance motivations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_underdetermination, conceptual, 'Whether remonstrance kernel instantiates two readings or one').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(remonstrance_authority__crown_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(remonstrance_crown_tr_t0, remonstrance_authority__crown_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(remonstrance_crown_tr_t20, remonstrance_authority__crown_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(remonstrance_crown_tr_t40, remonstrance_authority__crown_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(remonstrance_crown_be_t0, remonstrance_authority__crown_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(remonstrance_crown_be_t20, remonstrance_authority__crown_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(remonstrance_crown_be_t40, remonstrance_authority__crown_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(remonstrance_crown_su_t0, remonstrance_authority__crown_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(remonstrance_crown_su_t20, remonstrance_authority__crown_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(remonstrance_crown_su_t40, remonstrance_authority__crown_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(remonstrance_authority__crown_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(remonstrance_authority__crown_reading, 0.25).
narrative_ontology:affects_constraint(remonstrance_authority__crown_reading, remonstrance_authority__magistrate_reading).

% DUAL FORMULATION NOTE:
% The remonstrance_authority kernel decomposes into two structurally distinct readings with opposite victim/beneficiary alignments and opposite extraction direction vectors. The crown_reading (this story) treats magistrate remonstrance as illegitimate extraction mechanism. The magistrate_reading sibling story treats crown fiscal authority as illegitimate extraction mechanism, with remonstrance as the legitimate check. Both stories share the same base institutional mechanism (formal objection + enforcement delay) but assign legitimacy and extractiveness in opposite directions. The ε values will differ: the crown_reading exhibits high ε (0.68) for magistrate-directed extraction; the magistrate_reading will exhibit high ε for crown-directed extraction. The network link enables the engine to recognize that perspectival disagreement about 'who is extracting' is not measurement error but genuine structural symmetry: the same institutional mechanism is experienced as extraction by whoever lacks legitimate authority in the reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(remonstrance_authority__crown_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
