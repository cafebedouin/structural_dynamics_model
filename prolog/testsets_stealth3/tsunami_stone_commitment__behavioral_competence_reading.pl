% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__behavioral_competence_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Sanriku Tsunami Stone as Live Intergenerational Norm (Behavioral Competence Reading)
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   This story instantiates the behavioral_competence_reading of the
 *   tsunami_stone_commitment kernel: the inscribed warning stones of the
 *   Sanriku coast - erected after the 1896 Meiji tsunami that killed roughly
 *   22,000 people, recarved and reaffirmed after the 1933 Showa tsunami -
 *   treated as a LIVE normative commitment enforced across generations by
 *   custodial transmission, not as a weathered monument. Under this reading,
 *   the standing arrangement under contest is the stone's command together
 *   with the transmission practice that enforced it; epsilon is authored for
 *   THAT arrangement as this reading sees it (near-zero extraction, genuine
 *   mutual benefit), never for the heritage object the sibling readings argue
 *   about. ASSUMPTIONS STATED: (1) the real-world anchor is the Sanriku stone
 *   corpus, especially the Aneyoshi (Iwate) stone whose marked line the 2011
 *   Tohoku wave stopped beneath, leaving the hamlet's residents almost
 *   unharmed; (2) the interval maps t=0 to the first carvings (circa 1897)
 *   and t=120 to the present heritage era; (3) the manifest's expected delta
 *   labeled this reading 'piton' with very low epsilon, but the reading's own
 *   premise - retained live behavioral force - is incompatible with piton's
 *   defining atrophy, so the claim is refined to rope and the divergence is
 *   left for the engine to measure; the atrophy thesis belongs to the
 *   commemorative_husk_reading sibling, linked in
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   founding_survivor_carvers: agenda-setting authors (organized/constrained)
 *   - wrote the arrangement into stone from catastrophe; -
 *   transmission_elder_lineage: administering custodians
 *   (organized/identity_locked) - run the intergenerational enforcement; -
 *   elevation_heeding_households: primary beneficiaries
 *   (moderate/constrained) - sited at elevation, survived 2011; -
 *   shoreline_defying_households: cost-bearing defector seat
 *   (moderate/constrained) - declined the instruction and bore the water's
 *   cost; - emigrated_younger_descendants: absent voice (moderate/mobile) -
 *   exercised exit, thinning the transmitter pool; -
 *   prefectural_disaster_offices: institutional observer
 *   (institutional/analytical) - parallel protector, now primary; -
 *   disaster_ethnographers: analytical observer (analytical/analytical) -
 *   outside witness to whether enforcement was real; - heritage_curators:
 *   late-phase beneficiary (organized/mobile) - harvest the arrangement's
 *   archival value.
 *
 * KEY AGENTS:
 *   - founding_survivor_carvers: agenda-setting authors (organized power, constrained exit) - commissioned the inscriptions after the 1896 catastrophe and set the standing instruction
 *   - transmission_elder_lineage: administering custodians (organized power, identity_locked exit) - enforce the remembrance across generations; also beneficiaries via custodial standing
 *   - elevation_heeding_households: primary beneficiaries (moderate power, constrained exit) - accepted siting costs and survived the 2011 wave behind the stone's line
 *   - shoreline_defying_households: cost-bearing defector seat (moderate power, constrained exit) - built at the water's edge despite the inheritance; their losses came from the sea, not from the arrangement
 *   - emigrated_younger_descendants: absent voice (moderate power, mobile exit) - left for urban wages, draining the future transmitter pool while funding upkeep from afar
 *   - prefectural_disaster_offices: institutional observer (institutional power, analytical exit) - operate the seawalls, hazard maps, and warning systems that now perform the protective function
 *   - disaster_ethnographers: analytical observer (analytical power, analytical exit) - document transmission practice and carry the case into international memory studies
 *   - heritage_curators: late-phase beneficiaries (organized power, mobile exit) - maintain, exhibit, and teach the stones as heritage without administering any siting rule
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.07).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.1).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.07).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Sanriku Tsunami Stone as Live Intergenerational Norm (Behavioral Competence Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/institutional_memory").

domain_priors:requires_active_enforcement(tsunami_stone_commitment__behavioral_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, 'f125d2b1-0c67-42e1-b708-49f2177d1e19').
narrative_ontology:cs_kernel_codification('f125d2b1-0c67-42e1-b708-49f2177d1e19', fixed_text).
narrative_ontology:cs_authority_grounding('f125d2b1-0c67-42e1-b708-49f2177d1e19', lineage).
narrative_ontology:cs_interpretation_layer_present('f125d2b1-0c67-42e1-b708-49f2177d1e19').
narrative_ontology:cs_reading_relation('f125d2b1-0c67-42e1-b708-49f2177d1e19', tsunami_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_reading_relation('f125d2b1-0c67-42e1-b708-49f2177d1e19', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('f125d2b1-0c67-42e1-b708-49f2177d1e19', foundational, inscription_retained_binding_behavioral_force).
narrative_ontology:cs_axiom_status(inscription_retained_binding_behavioral_force, holdable).
narrative_ontology:cs_axiom_grounding('f125d2b1-0c67-42e1-b708-49f2177d1e19', inscription_retained_binding_behavioral_force, empirically_contingent).
narrative_ontology:cs_axiom('f125d2b1-0c67-42e1-b708-49f2177d1e19', foundational, compliance_sustained_by_intergenerational_enforcement).
narrative_ontology:cs_axiom_status(compliance_sustained_by_intergenerational_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('f125d2b1-0c67-42e1-b708-49f2177d1e19', compliance_sustained_by_intergenerational_enforcement, empirically_contingent).
narrative_ontology:cs_reference_frame('f125d2b1-0c67-42e1-b708-49f2177d1e19', ancestral_command_as_binding_law).
narrative_ontology:cs_drift_state('f125d2b1-0c67-42e1-b708-49f2177d1e19', post_2011_heritage_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f125d2b1-0c67-42e1-b708-49f2177d1e19', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, elevation_heeding_households).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, transmission_elder_lineage).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, heritage_curators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tsunami_stone_commitment__behavioral_competence_reading, shoreline_defying_households).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, precautionary_siting_principle).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__behavioral_competence_reading, durable_inscription_memory_reliability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Survivors of the 1896 Meiji Sanriku tsunami, which destroyed their villages and killed roughly a fifth of the regional population, commissioned and carved inscribed stones at the inundation line their generation witnessed. Some had already rebuilt shoreward after earlier floods and paid for it again in 1933; the stones encode that twice-paid tuition as a standing instruction to descendants. Fishing and farming tied them to the coast, so they could not simply leave; they spent their authority on stone instead.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, founding_survivor_carvers, agenda_setter,
    organized, generational, constrained, regional).

% Village elders and household heads who carry the remembrance duty: retelling the catastrophe at graves and festivals, walking children to the stone, declining to sell or build below the marked line, and shaming those who try. The duty passes with household headship; an elder who stops transmitting is seen as breaking faith with the dead of his own line. Their reward is standing - the community treats the rememberers as its moral archive - and their cost is the labor of annual repetition. Leaving the custodial role would mean leaving the village and the identity that comes with it.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, transmission_elder_lineage, agenda_setter,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__behavioral_competence_reading, transmission_elder_lineage, beneficiary).

% Households that sited and rebuilt their homes above the stone's line, accepting smaller plots, longer walks to the boats, and colder winter wind in exchange for sleeping above the reach of the waves their grandparents described. When the 2011 wave came, hamlets that had kept the rule lost almost no one; the water stopped at the old boundary. Leaving the coast entirely was possible but meant abandoning livelihood and ancestors alike, so nearly everyone stayed and complied.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, elevation_heeding_households, beneficiary,
    moderate, generational, constrained, local).

% Households that weighed the same inheritance and built at the water's edge anyway - for dock access, flat land, or disbelief that the sea would repeat itself within a lifetime. The arrangement pressed on them only as argument and social friction; the sea did the rest. Some family lines ended at the 1933 wave; others endured to lose homes and kin in 2011. What they bore came from the water, not from the stone.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, shoreline_defying_households, payer,
    moderate, biographical, constrained, local).

% Children and grandchildren raised on the transmission who left for schools and wages in Sendai and Tokyo. They carry the story but no longer stand in the circle where it is enforced, and their absence thins the pool of future elders. Many would have things to say about what the remembrance owes them - some send money home for the stone's upkeep - but they are not present when siting decisions are made.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, emigrated_younger_descendants, excluded,
    moderate, biographical, mobile, national).

% Prefectural and municipal governments that maintain seawalls, hazard maps, and the tsunami warning system. They record the stones as cultural property, cite them in education campaigns, and after 2011 documented how far the water reached relative to the old lines. They neither enforce the remembrance nor depend on it; their infrastructure now performs the protection the stone once urged.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, prefectural_disaster_offices, observer,
    institutional, generational, analytical, regional).

% Researchers in disaster anthropology and memory studies who interviewed transmitter lineages, mapped stone locations against inundation records, and carried the Sanriku stones into international circulation after 2011 as a canonical case of durable risk communication. Their accounts are the main outside witness to whether the enforcement was real.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, disaster_ethnographers, observer,
    analytical, biographical, analytical, global).

% Municipal museums, boards of education, and heritage programs that maintain the stones, mount exhibitions, and fold them into school curricula. They collect the arrangement's late value - visitors, grants, teaching material - without administering any siting rule. For them the stone's authority is already archival; its usefulness is as a story that persuades.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__behavioral_competence_reading, heritage_curators, beneficiary,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Converts a single generation's catastrophic experience into a durable, self-enforcing siting rule: individual households cannot retain multi-generation flood memory, and no market prices a once-per-century hazard, so the inscribed line plus annual re-transmission coordinates settlement placement above the remembered inundation boundary without any central authority or written code.
% TRANSFER_FUNCTION: Moves transmission labor and siting cost from each current generation toward its successors as reduced mortality risk; moves deference and standing toward the custodial elder lineages in exchange for their remembrance labor; moves no fees, rents, or goods to any administrator - the ledger is paid in obedience and paid out in survival.
% ABSENT_VOICES: The 1896 and 1933 dead set the stone's content and cannot revise it - the text froze at the moment of grief, so later knowledge (better engineering, revised inundation estimates) enters only through reinterpretation by the elder lineages. Shoreline-dependent fishers who wanted dockside plots objected in practice but never as a seated constituency; emigrated descendants are absent from every siting decision yet help fund the stone's upkeep; and the 2011 dead of non-heeding hamlets are the counterfactual witnesses no stone recorded.
% DISAPPEARANCE_RATIONALE: Remove the stones and the transmission duty overnight in, say, 1900, and settlement creeps seaward with each economic boom - exactly the trajectory visible in hamlets without stones - so the 1933 and 2011 waves find the same villages at the water's edge; casualty geography converges on the non-heeding baseline and the region's demographic recovery slows. The arrangements of every heeding household depend on the line having been drawn and repeated.
% FOUNDING_PROBLEM: After the 1896 Meiji Sanriku tsunami killed roughly 22,000 people and erased whole villages, survivors faced a problem no institution of their time solved: human memory of a catastrophic flood fades within two generations, exactly the recurrence interval of the hazard. The founding problem was to encode lived catastrophe durably enough to bind descendants who never saw the water.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: prefectural damage surveys and the post-2011 joint-survey literature recording inundation heights against stone lines, disaster-anthropology fieldwork on Sanriku remembrance practice, and municipal cultural-property registries - none of these seats collects from the arrangement. Beneficiary-side attestation (elders, curators) exists but is not relied upon; the fact that new warning stones were erected elsewhere in Japan after 2011 independently attests the founding problem's continued liveness.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__behavioral_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__behavioral_competence_reading, 0.07, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored very low (0.07 at interval end) because the arrangement transfers no rents: the compliance cost (smaller plots, longer walks to the boats) is the price of the good purchased, not a levy skimmed by any administrator. Suppression is low (0.10 at interval end) because compliance ran on social reinforcement and custodial authority, defection was continuously available and repeatedly taken, and exit by migration was always open - the arrangement trapped no one. Theater_ratio is authored at 0.38 (interval end) reflecting the late heritage-era shift: cleaning, exhibiting, and citing the stones is increasingly performative relative to a transmission practice that has thinned. Accessibility_collapse is moderate (0.55): once the premise is granted, building below the line is hard to defend, but land scarcity and harbor livelihood kept the alternative choosable, and some chose it. Resistance is low-moderate (0.2): episodic shoreward rebuilding after 1896 and 1933 and chronic youth indifference, but no organized opposition to the stones themselves. The three metric series run on ONE shared time grid (t = 0, 37, 60, 85, 105, 111, 120) so every metric is authored at every examined point; the compiler's union-grid substitution path is never exercised. The suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity change: machinery matures through the 1933 reaffirmation (ratchet to 0.35), normalizes postwar, decays under demographic attrition (0.16 by t=105), spikes briefly during the 2011 crisis re-transmission (0.24), and dissolves as state systems take over (0.10). The theater_ratio dips at t=111 (0.30 to 0.26) because the validation event temporarily re-functionalized the arrangement - real siting and evacuation decisions rode on inherited knowledge - before the heritage phase resumed; this is a crisis perturbation, not a cyclical extraction mechanism. Scalars in base_properties reflect the interval-end state per the corpus convention; the series carries the trajectory. Suppression is authored as a raw structural property and is NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the founding carvers' and elder lineages' position the arrangement is a sacred trust successfully kept - they experience it as duty vindicated in 2011. From the heeding households' position it is cheap insurance that paid out. From the defying households' position it was nagging obstruction followed, for the survivors among them, by bitter confirmation; they never experienced the arrangement as taking anything from them, only as reproaching them. The emigrated descendants barely register it as a live force at all - for them it is already the heritage object the husk sibling describes. The prefectural offices experience it as supplementary color to their own infrastructure. The engine computes these divergent per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   All declared beneficiaries sit near the beneficiary end of directionality: heeding households received survival for a modest siting cost; elder lineages received custodial standing in exchange for real transmission labor - compensation roughly commensurate with service, not captured rent; heritage curators collect archival value from an artifact that extracts from no one. NO victims are declared, and this omission is deliberate and load-bearing: the one seat that bore heavy costs (shoreline_defying_households) bore them from the HAZARD, not from the arrangement - the stone pressed on them only as argument and social friction. Listing them under victims would misattribute nature's toll to the constraint and inflate effective extraction; they are therefore seated as payers in stakeholders[] but excluded from the victims[] structural array. No directionality_overrides are authored: the override surface is keyed per power atom, and the two seats that would most need differentiation (heeding households and defying households) share the 'moderate' atom, so any override correcting one would distort the other. Local-to-regional scopes keep the engine's verification-difficulty amplification modest, consistent with the very low base epsilon.
 *
 * MANDATROPHY ANALYSIS:
 *   The R5 interview carries the obsolescence question: the founding problem (binding descendants who never saw the water against forgetting it) is authored LIVE and corroborated from outside the beneficiary set, so no dead-mandate-plus-persistence mismatch fires. Through the validation event the arrangement showed no mandate-outlived-function rot - it functioned as designed and then began handing its protective role to engineered infrastructure, which is transition, not atrophy. The classification discipline prevents two opposite mislabelings: a casual glance at an old stone nobody visibly obeys invites the inertial-debris reading (the husk sibling's territory, where a high theater_ratio and dissolved enforcement would compute a piton profile), while reverence for the stones' vindication invites canonizing them as natural law (a mountain claim this story refuses - emerges_naturally is false; the arrangement is a constructed commitment that demonstrably CAN decay, and the late-interval series shows the decay beginning). The rope claim with a rising theater series lets the engine date any transition toward the husk profile from data rather than from reverence or condescension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_sibling_contest,
    'Does the standing arrangement retain live behavioral force through active intergenerational enforcement, or has it decayed to a commemorative artifact whose past compliance was coincidental or weakly enforced (the commemorative_husk_reading''s thesis)?',
    'Settlement-pattern reconstruction from cadaster and elevation records 1897-2011, oral-history collection from transmitter lineages, and casualty-geography comparison between stone-heeding and non-heeding hamlets in the 1933 and 2011 events.',
    'If the husk sibling is correct, this story''s rope claim collapses toward an inertial, theatrically maintained profile, the epsilon referent shifts from enforced norm to maintained monument, and the vindicated propositions lose their warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_sibling_contest, empirical, 'Live-force versus commemorative-husk contest with the sibling reading over the same kernel.').

omega_variable(
    validation_axis_confounding,
    'Did the 2011 Tohoku outcomes validate the stone''s guidance specifically, or do engineered seawalls, official warnings, and topographic luck confound the binary test the catastrophe_validation_axis sibling proposes?',
    'Counterfactual inundation modeling that separates stone-guided siting effects from seawall and warning-system effects across matched hamlet pairs with and without inscriptions.',
    'Determines whether the vindicated propositions are genuinely vindicated or coincidentally correlated; a confounded test downgrades the validation sibling and tempers this reading''s external corroboration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(validation_axis_confounding, empirical, 'Whether the 2011 event functions as decisive validation evidence or as confounded correlation.').

omega_variable(
    attribution_to_inscription_vs_risk_culture,
    'Is the inscribed stone the operative carrier of the siting norm, or a mnemonic anchor riding on a broader post-catastrophe risk culture that would have persisted without any inscription?',
    'Compare hamlets with stones against matched hamlets with equivalent catastrophe exposure but no inscriptions, controlling for economic geography and harbor dependence.',
    'If attribution fails, the coordination function credited to the inscription is overstated and the arrangement is better modeled as one node in a wider norm cluster, changing the boltzmann coordination-type assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(attribution_to_inscription_vs_risk_culture, conceptual, 'Whether the stone itself carries the norm or merely marks a distributed risk culture.').

omega_variable(
    transmitter_identity_lock_decay,
    'Was the transmission chain''s late-interval thinning driven by structural depopulation alone, or also by dissolution of the elders'' custodial identity once state disaster systems assumed the protective role?',
    'Longitudinal oral history with transmitter-lineage descendants and participation records for remembrance observances from 1960 to 2020.',
    'If identity dissolution contributed alongside depopulation, the enforcement decay is partly internalized rather than purely structural, and the late theater_ratio rise overstates the loss of functional activity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmitter_identity_lock_decay, empirical, 'Structural versus internalized drivers of enforcement-capacity decay in the transmitter lineages.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsunami_behavioral_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t0, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t37, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 37, 0.12).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t37, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t60, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t60, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t85, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 85, 0.22).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t85, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t105, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 105, 0.3).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t105, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t111, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 111, 0.26).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t111, observed).
narrative_ontology:measurement(tsunami_behavioral_tr_t120, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 120, 0.38).
narrative_ontology:measurement_basis(tsunami_behavioral_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(tsunami_behavioral_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t0, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t37, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 37, 0.05).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t37, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t60, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 60, 0.05).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t60, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t85, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 85, 0.06).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t85, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t105, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 105, 0.06).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t105, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t111, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 111, 0.07).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t111, observed).
narrative_ontology:measurement(tsunami_behavioral_be_t120, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 120, 0.07).
narrative_ontology:measurement_basis(tsunami_behavioral_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsunami_behavioral_su_t0, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t0, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t37, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 37, 0.35).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t37, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t60, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t60, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t85, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 85, 0.22).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t85, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t105, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 105, 0.16).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t105, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t111, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 111, 0.24).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t111, observed).
narrative_ontology:measurement(tsunami_behavioral_su_t120, tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 120, 0.1).
narrative_ontology:measurement_basis(tsunami_behavioral_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the tsunami stones' (kernel: tsunami_stone_commitment). The label conflates three structurally distinct claims with different epsilon values and different failure modes: (1) THIS story, the behavioral_competence_reading - the inscription retained live behavioral force through active intergenerational enforcement (very low epsilon, genuine coordination); (2) the catastrophe_validation_axis - the 2011 tsunami constitutes decisive binary validation evidence (an evidentiary claim that CONSUMES this reading's premise as input); (3) the commemorative_husk_reading - the inscription decayed to a symbolic artifact with coincidental compliance (an atrophy thesis that directly contradicts this reading over the same referent). Per the epsilon-invariance principle these are separate stories, linked here; the upstream behavioral-force claim is cited as evidence by the validation-axis claim, and contested by the husk claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
