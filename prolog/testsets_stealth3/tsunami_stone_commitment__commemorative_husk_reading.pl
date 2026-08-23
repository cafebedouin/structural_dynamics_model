% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Warning Stones as Commemorative Husk (Behavioral-Force-Collapsed Reading)
 *   domain: disaster_anthropology/commitment_systems/institutional_memory
 *
 * SUMMARY:
 *   Along the Sanriku coast, stone stelae erected after the 1896 and 1933
 *   tsunamis mark the reach of past waters and instruct descendants not to
 *   build below them. Under this reading — the commemorative husk — those
 *   inscriptions no longer bind anyone. Compliance with the marker lines,
 *   where it occurred, was coincidental (terrain, economics, or harbor
 *   geography made the high ground preferable anyway) or weakly enforced by
 *   aging village norms that expired with the generations that remembered the
 *   water; the postwar growth decades filled the low ground below the stones,
 *   and the 2011 tsunami reached or passed the old marks exactly there. What
 *   survives is a venerated object: cleaned, plaqued, ceremonialized, taught
 *   — and planning-irrelevant. The standing arrangement this story is ABOUT,
 *   and the sole referent of its epsilon, is that arrangement: stones
 *   maintained as heritage while development proceeds beneath their
 *   instructions, transferring catastrophe exposure onto residents who do not
 *   yet exist. CONSTRAINT FAMILY NOTE: the colloquial label 'the tsunami
 *   stones' covers structurally distinct claims and is decomposed per the
 *   epsilon-invariance principle. This file holds the
 *   behavioral-FORCE-collapsed reading (epsilon high: non-protection licenses
 *   development). The sibling behavioral_competence_reading holds the
 *   force-retained reading (epsilon low: enforced avoidance is the
 *   arrangement). The catastrophe_validation_axis holds the 2011 event as a
 *   binary test of the stones' ACCURACY — orthogonal to force, compatible
 *   with either. Each reading authors its own file; this one links both. KEY
 *   AGENTS (by structural relationship): - municipal_heritage_authorities:
 *   Agenda-setter (organized/constrained) — administers the stones as
 *   heritage and has decided they mean memory - economic_development_actors:
 *   Primary beneficiary (powerful/arbitrage) — books the calm-interval gains
 *   of building below the markers - public_works_engineering_complex:
 *   Secondary beneficiary (institutional/constrained) — collects the mandate
 *   the stones' silence creates - survivor_memory_keepers: Identity-fused
 *   maintainer (moderate/identity_locked) — tends the stones as who they are,
 *   not what they do - future_coastal_residents: Primary target
 *   (powerless/trapped) — inherits the exposure and holds no seat -
 *   hazard_researchers: Excluded expert voice (moderate/mobile) — documents
 *   the gap, moves no setback line - disaster_archaeologists: Analytical
 *   observer (analytical/analytical) — sees the full arc across inscription
 *   cohorts
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.76).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.42).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.76).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.82).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Warning Stones as Commemorative Husk (Behavioral-Force-Collapsed Reading)").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "disaster_anthropology/commitment_systems/institutional_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, 'a858cb9b-ffe3-4937-b169-3c8e91d0f08c').
narrative_ontology:cs_kernel_codification('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', fixed_text).
narrative_ontology:cs_authority_grounding('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', lineage).
narrative_ontology:cs_interpretation_layer_present('a858cb9b-ffe3-4937-b169-3c8e91d0f08c').
narrative_ontology:cs_reading_relation('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', tsunami_stone_commitment__behavioral_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', tsunami_stone_commitment__catastrophe_validation_axis, influences).
narrative_ontology:cs_axiom('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', foundational, inscription_force_has_collapsed).
narrative_ontology:cs_axiom_status(inscription_force_has_collapsed, holdable).
narrative_ontology:cs_axiom_grounding('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', inscription_force_has_collapsed, empirically_contingent).
narrative_ontology:cs_axiom('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', secondary, commemoration_is_not_protection).
narrative_ontology:cs_axiom_status(commemoration_is_not_protection, holdable).
narrative_ontology:cs_axiom_grounding('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', commemoration_is_not_protection, instrumental).
narrative_ontology:cs_reference_frame('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', ancestral_binding_injunction).
narrative_ontology:cs_drift_state('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', post_2011_commemorative_era, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('a858cb9b-ffe3-4937-b169-3c8e91d0f08c', '2026-08-05T09:30:00Z').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, economic_development_actors).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, public_works_engineering_complex).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, survivor_memory_keepers).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the stones as designated cultural properties: budget for cleaning and plaque upkeep, organize annual memorial ceremonies, host school visits, and register the sites with prefectural and national heritage lists. In practice they decide what the stones are for, and have settled on memory. Their offices sit on high ground above the marker lines and their tenures are shorter than the sea's worst intervals. Redirecting the inscriptions into binding setback rules would mean fighting landowners, developers, and chambers of commerce over land values, for a benefit that arrives, if at all, decades after their terms end.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, municipal_heritage_authorities, agenda_setter,
    organized, biographical, constrained, regional).

% Build and operate hotels, guesthouses, seafood processing plants, marinas, and seaside subdivisions on the flat land below the old high-water markers, where the roads, ports, and harbors already are. Capital turns over in five-to-thirty-year cycles while the sea's worst intervals run longer than a career; gains are booked long before any recurrence, and losses, when they come, are partly met by national reconstruction budgets and insurance pools. Moving upslope would mean paying hillside premiums for parcels without port access; staying is cheaper every year the sea stays quiet.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, economic_development_actors, beneficiary,
    powerful, immediate, arbitrage, regional).

% Design, build, and maintain seawalls, breakwaters, elevated roadbeds, and evacuation platforms — the engineered answer that replaced the stones' answer. Each budget cycle and each catastrophe enlarges the mandate, and the apparatus employs the planners, contractors, and coastal engineers whose careers span the gap between disasters. Its plans assume people remain on the low ground behind the walls, which is the opposite of what the inscriptions asked for; few inside the ministries read the stones as instructions.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, public_works_engineering_complex, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, public_works_engineering_complex, agenda_setter).

% Elders, priests, and descendants of the drowned who tend the stones, lead the walking tours, tell the stories at funerals and festivals, and appear in documentaries each March. Their standing in the community rests on being the ones who remember; the role is who they are, not a job they hold. Their numbers thin every year — the 1933 generation is gone, and the 1896 stories survive mainly in the stones themselves — while the young attend the ceremonies the way one attends any obligation. Asking them to lead a fight over zoning lines would ask storytellers to become politicians; most would rather keep the stories warm.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, survivor_memory_keepers, beneficiary,
    moderate, biographical, identity_locked, local).

% Not yet born, or children now: the people who will live in the houses, staff the plants, and send their kids to the schools currently being placed below the marker lines. They hold no seat in any assembly, cast no vote on setbacks, and cannot decline the exposure they are being handed. When the interval closes they will inherit both the water and the plaques explaining, in respectful language, that all of this had happened before.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, regional).

% Geomorphologists, historians, and disaster scholars who have surveyed the stones, mapped the 2011 inundation against them, and published the finding that the water reached or passed the old marks precisely where towns had grown downhill. They testify at symposiums, advise heritage candidacies, and watch the planning committees from outside the room; their reports circulate in journals the assemblies never read. Nothing pins them to any one coast — the next conference is always abroad — and their findings change no setback line by themselves.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, hazard_researchers, excluded,
    moderate, generational, mobile, global).

% Study the stones as a dataset: three centuries of attempts to write a warning durable enough to outlive its own language. They compare inscription cohorts, erosion rates, and settlement patterns across the Pacific rim, and report to no one in particular. Their seat sees the full arc — erection, obedience, forgetting, ceremony — without any stake in what the next cohort decides.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, disaster_archaeologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__commemorative_husk_reading, economic_development_actors).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Synchronizes communal mourning and identity around the disaster dead: fixed calendar rites, shared sites, a common account of who the community is. Secondarily, it settles — symbolically — the question the stones originally posed about where building may go, lowering the political cost of lowland development by moving the answer from zoning chambers to ceremony grounds.
% TRANSFER_FUNCTION: Transfers catastrophe exposure from the present to the future: land value and operating profit on hazard-zone parcels flow to today's builders and operators, while mortality and asset risk accumulate for residents not yet born; reconstruction costs, when the interval closes, move onward to national taxpayers.
% ABSENT_VOICES: The authors of the stones — the drowned of 1896 and 1933 — cannot object, and the people who will live below the markers do not yet exist to object. Hazard researchers and relocation advocates hold findings and proposals but sit outside the planning chambers; their absence is procedural rather than accidental, since the committees that decide setbacks are staffed by landowners, business associations, and municipal boards drawn from the development economy.
% DISAPPEARANCE_RATIONALE: What would rearrange is the commemorative economy and the communities' self-account: ceremonies would lose their anchor sites, museums and school curricula their core exhibits, survivor families their appointed places of grief. What would NOT rearrange is land use — no setback would move, no house would be lifted, because the stones already bind no one. That asymmetry is this reading's central evidence: a live instruction's removal would change where people build; a husk's removal changes only the liturgy.
% FOUNDING_PROBLEM: After the 1896 and 1933 Sanriku tsunamis killed tens of thousands, surviving communities faced a transmission problem: the sea's worst intervals run longer than individual memory, so each generation rebuilds on the flat shore as the horror fades. The stones were cut to carry one instruction across that gap — mark the water's reach, and do not build your homes below it.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: municipal cultural-property registries classify the stones as folk materials and memorial assets, not planning instruments; post-2011 geomorphological surveys published outside the region found 2011 inundation reaching or exceeding the old marker lines precisely where towns had rebuilt downhill; and prefectural reconstruction plans from 2011–2015 allocate setback decisions to engineered defenses and buyout programs with no reference to the inscriptions. No party that profits from the current arrangement attests the founding problem is live; the attestations that exist come from archives, surveys, and registries with no stake in coastal land values.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.76, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.76 because the referent arrangement transfers the century's largest tail risk onto a constituency with no seat: the value of lowland parcels is consumed now, the mortality and asset destruction arrive later, and the 2011 realization (measured at 0.88 on the series) showed the transfer cashing out at scale. Suppression is 0.42 and deliberately split in explanation: the ACTIVE enforcement force has decayed to near nil (the suppression_requirement series falls 0.55 to 0.09 — tracing enforcement-capacity decay is precisely this story's dynamic, which is why that series is authored), but the arrangement's suppressive character persists structurally — dissenting expert seats are procedurally outside the planning chambers, the payer seat is constitutionally empty until the catastrophe fills it, and sunk lowland development forecloses cheap reversal. Theater_ratio 0.82 is the husk's signature: cleaning, plaques, ceremonies, school visits, and documentary appearances dominate all activity around the stones, while functional use (setting a single setback line from an inscription) is approximately zero. Accessibility_collapse is LOW (0.30) because the husk forecloses nothing — engineering substitutes (walls, warnings, buyouts) remain fully available and were in fact built; the husk's decay is what left room for them. Resistance 0.40 reflects real but marginal contestation: researchers publish, some survivors' groups press relocation, most residents are passive. The series run on one shared nine-point grid (every tracked metric authored at every point, per the alignment rule). CYCLICAL PATTERN: the 2011 dip in theater_ratio (0.74 to 0.55) and the small suppression blip (0.08 to 0.16) are the crisis phase of a catastrophe-cycle oscillation — each disaster briefly re-functionalizes memory (drills, relocation zones, renewed attention), then the energy drains back into ceremony as budgets shift to concrete. The oscillation is partly an extraction mechanism in the intermittent-reinforcement sense: the post-catastrophe surge of 'never again' feeling is discharged through commemoration rather than converted into binding land-use rules, resetting the husk for another interval. CLAIM/METRIC INDEPENDENCE: claimed_type is piton on structural grounds (function atrophied, persistence inertial and theatrical, administrator could re-anchor the stones into zoning but bears almost none of the catastrophe cost relative to the political cost of trying) while the metrics describe heavily extractive operation — the divergence is intentional and is the datum.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the heritage-authority seat the stones are cherished assets: budgeted, curated, identity-bearing, and costing nothing but care. From the development seat they are picturesque furniture — a scenic backdrop that imposes no setback and thereby leaves the flat land buildable. From the keeper seat the stones are selfhood. From the researcher seat they are a documented failure of transmission. From the payer seat — the deepest gap in the story — they are nothing at all yet, because that seat is temporally displaced: the people who bear the extraction cannot hold the seat until they exist, at which point the arrangement that positioned them will be decades past amendment. Two same-power moderates diverge sharply on exit: the keepers are identity_locked (their role is relational and institutional selfhood — 'the ones who remember' — and exit would dissolve who they are), while the researchers are mobile (expertise travels; the next conference is abroad). Inter-institutionally, the heritage apparatus and the engineering complex are both organized/institutional actors at nominal parity, yet the constraint subsidizes one with meaning and the other with mandate, and neither experiences it as a cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation and no directionality_overrides are authored. economic_development_actors and public_works_engineering_complex are declared beneficiaries with arbitrage/constrained exits — d sits near the beneficiary end, and the engineering complex's collection is indirect (mandate and budget rather than land value), which the derivation's exit modulation captures. survivor_memory_keepers are declared beneficiaries but their identity_locked exit and their own residence on the exposed coast pull their d up from pure-beneficiary toward the middle — they collect meaning AND carry residual skin in the game. future_coastal_residents are the declared victim with trapped exit and generational horizon — d sits at the full-target end, amplified by scope. municipal_heritage_authorities (agenda-setter) and hazard_researchers (excluded) are intentionally left undeclared so the engine's fallback supplies their d; the commentary records the intended relationships (authority near-beneficiary at roughly 0.15, researchers near-symmetric) rather than forcing them through overrides, because overrides key on the power atom and both keepers and researchers hold 'moderate' — an override would clobber the keepers' correctly derived beneficiary-side d to fix the researchers'. Scope amplification runs through the payer seat: the exposure is regional, verification of compliance is effectively impossible across a century, and the victim cannot verify anything at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate — protect descendants by carrying one instruction across the memory gap — has wholly outlived its function: the instruction is no longer issued to anyone with authority to heed it. mandatrophy_resolved is declared true, and the R5 mismatch consumer should fire here by design: founding_problem_status=dead combined with disappearance_verdict=world_rearranges is the zombie signature, cross-checked against the computed piton path and the 0.82 theater ratio. Claiming piton prevents two mislabelings at once. Against the sentimental reading (rope: 'the stones still coordinate us'), it insists that coordinating grief is not coordinating land use, and the disappearance rationale makes the test explicit — remove the stones and no setback moves. Against the accusatory reading (snare: 'the stones are a scam'), it notes that nobody OPERATES this extraction: no committee meets to keep the markers binding-free, no enforcement suppresses exits; the gains flow through the arrangement's non-operation, which is the piton signature. The honest complication — that development actors demonstrably capture the calm-interval gains and that fixing is prohibitive for whoever could fix it, making the receipt surface read snare-flavored even while the persistence mechanism is inertial — is left open as the piton_snare_boundary omega rather than resolved by fiat. Coalition check: the victim class cannot coalition at all, since its members are not yet present; the only feasible coalition is present-tense (researchers plus relocation advocates plus keepers breaking identity frame), and its formation is exactly what the identity-lock and procedural exclusion suppress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of kernel tsunami_stone_commitment — the commemorative_husk_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'The behavioral_competence_reading would instantiate the same stones with retained force: enforced avoidance enters the arrangement, epsilon drops sharply, and the type migrates toward a live coordination mechanism. The catastrophe_validation_axis re-keys evaluation to accuracy evidence (did the markers record the true reach?) rather than force (did they bind?), and is logically compatible with either force reading. The disagreement is located in a single parameter: whether the transmitted instruction still binds conduct.',
    'Classification is indexical to the reading: this file''s high epsilon is a property of the husk reading''s arrangement, not of ''the tsunami stones'' simpliciter. Cross-reading comparison is valid only through the kernel join, never by merging the files.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which kernel, which reading, what siblings would change, where the dispute lives.').

omega_variable(
    piton_snare_boundary,
    'Does the husk persist inertially while gains flow around it (piton), or do concentrated beneficiaries depend on its non-function enough to constitute capture (snare)?',
    'Trace whether development interests actively defend the husk state — lobbying against marker-anchored zoning, funding candidates who oppose setback ordinances, commissioning reassurance studies — versus merely exploiting it passively. Active defense of the non-function is the capture signature.',
    'A snare resolution raises computed effective extraction for the beneficiary seats and shifts the remedy class from revival (re-anchoring the stones into law) to dismantling-and-substitution (binding zoning independent of the stones). A piton resolution keeps the remedy at revival, aimed at the administrator''s cost-asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(piton_snare_boundary, empirical, 'Whether the arrangement''s persistence serves a concentrated beneficiary or merely persists while gains accrue.').

omega_variable(
    coincidental_compliance_attribution,
    'Was historical compliance with the marker lines coincidental (terrain and economics already favored the high ground) or weakly enforced (real norm enforcement that later decayed)?',
    'Archival land-transaction and permit records compared across communities with and without documented enforcement practices (elder veto of lowland plots, village sanctions), plus oral-history collections distinguishing remembered enforcement from remembered preference.',
    'If compliance was ever enforcement-backed, the decay narrative is confirmed and the husk reading''s epsilon is attributable to a lost function — revival is meaningful. If compliance was always coincidental, the commitment never had force, epsilon was high from erection, and the stones are better read as aspirational text than as a degraded mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coincidental_compliance_attribution, empirical, 'Attribution of the compliance record: coincidence versus decayed enforcement.').

omega_variable(
    false_assurance_magnitude,
    'How much of the measured extraction flows through false assurance — residents trusting walls plus honored memory in place of relocation — as opposed to plain market discounting of a distant tail risk?',
    'Risk-perception surveys and behavioral comparison across communities with strong versus weak stone traditions: insurance uptake, elevation choices, relocation willingness, controlling for seawall presence and subsidy schedules.',
    'If false assurance carries a large share, the husk is not merely inert but actively harmful — its ceremonial maintenance suppresses the demand for protection — and the highest-leverage fix is explicit disavowal rather than revival. If discounting dominates, the stones are epiphenomenal and the extraction is ordinary temporal arbitrage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(false_assurance_magnitude, empirical, 'Share of extraction routed through the assurance function of commemoration.').

omega_variable(
    post_catastrophe_revival_durability,
    'Is the post-2011 revival (evacuation drills, restricted reconstruction zones, some hillside relocations) a durable re-institutionalization of the stones'' instruction, or the crisis-phase flicker the cycle predicts?',
    'Longitudinal land-use data 2015–2035: whether setback lines hold as reconstruction budgets normalize, whether relocation-zone residency persists across a generation, whether drill participation decays on the observed post-1933 curve.',
    'If flicker, the husk state re-consolidates and the extractiveness series resumes climbing toward the next realization. If durable, this reading''s classification migrates toward a transitional support with a real sunset on the old arrangement, and the sibling behavioral_competence_reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_catastrophe_revival_durability, empirical, 'Durability of the crisis-phase revival against the historical forgetting curve.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 1955, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t1955, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1955, 0.25).
narrative_ontology:measurement(tsun_tr_t1965, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1965, 0.33).
narrative_ontology:measurement(tsun_tr_t1975, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1975, 0.45).
narrative_ontology:measurement(tsun_tr_t1985, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1985, 0.58).
narrative_ontology:measurement(tsun_tr_t1995, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 1995, 0.68).
narrative_ontology:measurement(tsun_tr_t2005, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2005, 0.74).
narrative_ontology:measurement(tsun_tr_t2011, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2011, 0.55).
narrative_ontology:measurement(tsun_tr_t2015, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2015, 0.7).
narrative_ontology:measurement(tsun_tr_t2025, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 2025, 0.82).

% Extraction over time
narrative_ontology:measurement(tsun_be_t1955, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1955, 0.35).
narrative_ontology:measurement(tsun_be_t1965, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(tsun_be_t1975, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1975, 0.52).
narrative_ontology:measurement(tsun_be_t1985, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(tsun_be_t1995, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 1995, 0.66).
narrative_ontology:measurement(tsun_be_t2005, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(tsun_be_t2011, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2011, 0.88).
narrative_ontology:measurement(tsun_be_t2015, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2015, 0.72).
narrative_ontology:measurement(tsun_be_t2025, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 2025, 0.76).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t1955, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1955, 0.55).
narrative_ontology:measurement(tsun_su_t1965, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1965, 0.46).
narrative_ontology:measurement(tsun_su_t1975, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1975, 0.36).
narrative_ontology:measurement(tsun_su_t1985, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1985, 0.27).
narrative_ontology:measurement(tsun_su_t1995, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 1995, 0.18).
narrative_ontology:measurement(tsun_su_t2005, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2005, 0.11).
narrative_ontology:measurement(tsun_su_t2011, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2011, 0.08).
narrative_ontology:measurement(tsun_su_t2015, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2015, 0.16).
narrative_ontology:measurement(tsun_su_t2025, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 2025, 0.09).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, behavioral_competence_reading).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, catastrophe_validation_axis).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel tsunami_stone_commitment decomposes per the epsilon-invariance principle into readings that differ on one structural parameter — the behavioral force of the transmitted inscription. This file (commemorative_husk_reading) authors epsilon against the force-collapsed arrangement: stones as heritage, development beneath the markers, exposure transferred to future residents (epsilon 0.76, claimed piton). The sibling behavioral_competence_reading authors the force-retained arrangement (enforced avoidance; low epsilon; coordination-forward type). The catastrophe_validation_axis authors the 2011 event as a binary accuracy test — orthogonal to force, hence compatible with either sibling, and linked to both. The upstream/downstream structure runs from the validation axis (empirical evidence both readings cite) into the force dispute; this reading links both siblings via affects_constraints and neither sibling's verdicts are folded into this file's epsilon, referent, or stakeholder set.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
