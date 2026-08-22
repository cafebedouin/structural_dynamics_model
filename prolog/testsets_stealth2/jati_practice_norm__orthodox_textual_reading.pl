% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Textual Jati Boundary Regime (Scriptural Varna Fixity with Pollution Sanction)
 *   domain: social/religious/political-economic
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the jati_practice_norm kernel: the
 *   orthodox textual reading, on which jati boundaries derive from a fixed
 *   scriptural varna framework and deviation from assigned position
 *   constitutes ritual pollution. The standing arrangement under contest —
 *   the arrangement this story is about — is that orthodox order itself:
 *   hereditary occupational assignment, pollution sanctions on transgression,
 *   and blocked mobility, with priestly scriptural elites and dominant
 *   landowning castes positioned as beneficiaries and polluting-occupation
 *   and servile jatis positioned as cost-bearers. Per the ε-invariance
 *   decomposition rule, the colloquial label 'the caste system' covers
 *   structurally distinct claims; this reading is one of three sibling
 *   constraints (localized_practice_reading: boundaries as continuously
 *   renegotiated coordination norms; colonial_census_reading: rigidity as
 *   administrative reification), each with its own ε, beneficiaries, and
 *   classification, linked through network.affects_constraints. This story
 *   does not average across readings: its ε is authored for the orthodox
 *   constituted arrangement alone, and it is high because the reading's own
 *   content — pollution doctrine, birth-assigned occupation, ontological
 *   boundary — is the transfer mechanism.
 *
 * KEY AGENTS:
 *   - brahmin_scriptural_elites: agenda-setting seat (institutional power / identity-locked exit) — interprets the shastras, adjudicates purity and boundary disputes, collects ritual fees and ceremonial gifts; their authority exists only inside the fixed framework.
 *   - dominant_landowning_castes: primary beneficiary seat (powerful / constrained exit) — commands hereditary labor and service from lower jatis and enforces local boundaries through caste panchayats; collects the material surplus the pollution system secures.
 *   - polluting_occupation_jatis: primary target seat (powerless / trapped exit) — assigned scavenging, leatherwork, corpse-handling; denied temple, well, and school access; status and occupation hereditary; exit blocked because pollution follows the mover and conversion historically did not dissolve the stigma.
 *   - hereditary_servile_jatis: secondary target seat (powerless / trapped exit) — bound labor and tenancy fixed by birth; mobility blocked by debt, custom, and violence.
 *   - intermediate_service_jatis: dual-positioned seat (moderate / constrained exit) — hereditary service providers who pay status costs upward and enforce purity boundaries against those below; their own standing depends on maintaining the boundary beneath them.
 *   - anti_caste_reform_movements: excluded seat (organized / constrained exit) — Satyashodhak, self-respect, and Ambedkarite currents that deny the framework's scriptural authority; the orthodox frame gives them no adjudicative standing and answers them with sanction.
 *   - constitutional_framers: analytical observer seat (institutional / analytical exit) — the post-colonial drafting body that surveyed untouchability's operation and dismantled the arrangement's state enforcement at the interval's end.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.86).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.88).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.86).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Textual Jati Boundary Regime (Scriptural Varna Fixity with Pollution Sanction)").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social/religious/political-economic").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, 'a42cf7d5-9a18-48d0-b687-71f56d3c6eb9').
narrative_ontology:cs_kernel_codification('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', fixed_text).
narrative_ontology:cs_authority_grounding('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', extraction).
narrative_ontology:cs_interpretation_layer_present('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9').
narrative_ontology:cs_reading_relation('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', foundational, varna_birth_determines_dharma).
narrative_ontology:cs_axiom_status(varna_birth_determines_dharma, holdable).
narrative_ontology:cs_axiom_grounding('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', varna_birth_determines_dharma, theological).
narrative_ontology:cs_axiom('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', foundational, deviation_constitutes_ritual_pollution).
narrative_ontology:cs_axiom_status(deviation_constitutes_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', deviation_constitutes_ritual_pollution, theological).
narrative_ontology:cs_reference_frame('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', shastric_varna_fixity).
narrative_ontology:cs_drift_state('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', constitutional_abolition_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a42cf7d5-9a18-48d0-b687-71f56d3c6eb9', '2026-08-04T00:00:00Z').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_scriptural_elites).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, polluting_occupation_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, hereditary_servile_jatis).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, intermediate_service_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, intermediate_service_jatis).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, varna_scriptural_fixity).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, karmic_desert_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, ritual_purity_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the scriptural corpus, adjudicate purity and boundary disputes, and fix the terms on which jatis may interact, marry, and approach ritual space. Collect ritual fees, ceremonial gifts, and first claim on honorifics. Their standing exists only inside the fixed framework — livelihood, authority, and self-understanding are all constituted by the texts they administer, so leaving the framework would mean ceasing to be what they are.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_scriptural_elites, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, brahmin_scriptural_elites, beneficiary).

% Own the land and command the hereditary labor and service of lower jatis; run the local caste panchayats that punish boundary transgression with fine, boycott, and expulsion. They collect the material surplus the pollution system secures — labor discipline without wage bargaining. They do not run the scriptural apparatus but fund and enforce it; exit would mean surrendering the labor supply and local dominance the framework guarantees them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes, beneficiary,
    powerful, generational, constrained, regional).

% Assigned by birth to scavenging, leatherwork, and corpse-handling; denied entry to temples, access to upper-caste wells, and schooling; paid in grain scraps or not at all for obligatory village duties. Their status follows them through space and survives conversion attempts; moving to a town offers partial anonymity but no change in occupation or stigma. Transgression is punished as pollution, and their testimony has no standing in the bodies that adjudicate the rules that bind them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, polluting_occupation_jatis, payer,
    powerless, generational, trapped, continental).

% Bound by birth into agricultural labor and tenancy on dominant-caste land; obligations fixed by custom and reinforced by debt and the threat of violence. They may not refuse service, renegotiate terms, or leave the village without forfeiting subsistence. They retain ritual approach to some shared spaces the polluting jatis lose entirely, but their mobility is as fully blocked.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, hereditary_servile_jatis, payer,
    powerless, generational, trapped, regional).

% Hereditary providers of services to upper jatis — washing, barbering, midwifery, message-running. They pay status costs to those above and collect deference and exclusive service rights from those below; their livelihood depends on refusing service to polluting jatis, so they police the boundary that also presses on them. Exit would cost them their patronage network above and their service monopoly below.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, intermediate_service_jatis, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, intermediate_service_jatis, beneficiary).

% Organized currents — Satyashodhak, self-respect, Ambedkarite — that deny the framework's scriptural authority and organize refusal of priestly ritual, temple entry, and occupational heredity. The orthodox frame assigns them no adjudicative standing: their objections are answered as ignorance of dharma, and their members face boycott and violence. They speak about the arrangement from outside every body authorized to change it.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, anti_caste_reform_movements, excluded,
    organized, generational, constrained, regional).

% The constitution-drafting body that surveyed untouchability's operation, heard deposition from the affected communities, and wrote the abolition of untouchability into law at the interval's end. They hold no position inside the framework; their remedies — legal abolition and state enforcement against private sanction — are what dismantled the arrangement's enforcement machinery in the interval's final years.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, constitutional_framers, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, dominant_landowning_castes).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Orders interdependence among hereditary occupational and ritual communities across a subcontinent without centralized administration: it fixes who performs which ritual, service, and productive functions, regulates marriage pools and commensality, and supplies a self-enforcing compliance mechanism through pollution doctrine — each community's position and duties are known without negotiation.
% TRANSFER_FUNCTION: Moves hereditary labor, agricultural produce shares, ritual service fees, and deference from polluting-occupation and servile jatis to priestly elites and dominant landowning castes; moves pollution risk and menial burden downward; restricts lower jatis' access to temples, wells, schools, and markets.
% ABSENT_VOICES: The polluting-occupation jatis live inside the arrangement but have no adjudicative standing in it — scriptural interpretation is monopolized by the twice-born varnas, so those bearing the pollution burden cannot contest its terms within the frame. Anti-caste reformers object from outside and are dismissed as ignorant of dharma; women bound by purity rules likewise lack standing in the adjudicating bodies.
% DISAPPEARANCE_RATIONALE: Village service economies, marriage pools, ritual provision, labor tenancy, and temple administration are all organized by the boundary system. An overnight disappearance would force simultaneous reorganization of labor contracting, ritual services, marriage matching, and local political authority: the orthodox seats would lose their income streams and offices, the dominant castes would lose their labor discipline, and the payer seats' obligations would dissolve into negotiable contracts.
% FOUNDING_PROBLEM: Ordering a large, differentiated population of occupational and ritual specialists into a stable division of labor and ritual hierarchy in the absence of centralized administrative capacity — the scriptural varna framework answered who does what, who may marry whom, and who may approach whom, with pollution doctrine supplying enforcement where no state capacity existed.
% FOUNDING_PROBLEM_CORROBORATION: Religious-historical scholarship outside the beneficiary set (textual and epigraphic studies of dharmashastra and early polities) attests the framework's original function as ordering ritual and occupational division of labor. Anti-caste movement testimony, also from outside the benefiting parties, attests the founding problem is obsolete: modern contract, state administration, and voluntary association perform the ordering function without pollution enforcement. No source outside the beneficiary seats corroborates the orthodox claim that the pollution solution remains necessary.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.86, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.86, authored at the arrangement's mature orthodox peak, c. 1890 on the interval grid) because the transfer is hereditary and total: occupation, service obligation, ritual access, and marriage pool are all fixed at birth, and the surplus moves without wage bargaining, contract, or exit. Suppression is higher still (0.88) because persistence depends on actively punishing transgression — boycott, fine, expulsion, ritual excommunication, private violence — administered by caste panchayats under scriptural warrant. The suppression_requirement series shows the enforcement ratchet (rising under reform pressure to the 1890s peak) and then collapse after constitutional abolition, which is why the end-of-interval value (0.45) sits far below the characteristic scalar. Theater is low-to-moderate at the peak (0.28) — the ritual and service functions are real inside the frame — and rises steadily to 0.55 as enforcement decays and purity observance becomes performative boundary maintenance. Accessibility collapse is 0.70: alternatives (conversion, sect movement, urban migration) partially collapse — conversion historically did not dissolve stigma, and the pollution doctrine follows the mover — but do not collapse completely, which is why resistance (0.60) is substantial: bhakti-era dissent, Satyashodhak and self-respect organizing, temple-entry satyagraha, and Ambedkarite conversion all met the arrangement and were met with enforcement intensification. Claim and metrics are authored independently: claimed_type snare is asserted from the structural reading (a coordination story — ordering occupational and ritual interdependence — operating as cover for hereditary transfer with named victims and suppressed exits); the metrics are asserted from the historical record. Base properties characterize the arrangement at its mature orthodox phase, not its post-abolition end state; the measurement series carries the full arc on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setting seat compute differently from the same structure. From the brahmin scriptural seat, the framework is cosmic order it did not create but administers — the transfer it collects is dana, the boundaries it adjudicates are revelation; nothing in its situation reads as illicit. From the trapped payer seats, the same structure is total: occupation, subsistence, worship, and marriage are all gated by a rule they cannot contest because their testimony has no standing. The intermediate service jatis split: they pay upward and collect downward, so their seat should compute near-symmetric or bifurcated. The engine computes this divergence from power, exit, and role data; the authored snare claim does not adjudicate it — it names the structure from the analytical seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Brahmin scriptural elites sit near the beneficiary end (d low): the framework subsidizes their authority, fees, and interpretive monopoly. Dominant landowning castes also sit near the beneficiary end — they collect the material surplus without running the scriptural apparatus. Polluting-occupation jatis sit near the full-target end (d high): they bear the transfer with trapped exit and continental scope, so effective extraction is amplified for them twice — by directionality and by scope, since verification of boundary enforcement is hardest at continental reach. Hereditary servile jatis are similarly near-full targets at regional scope. Intermediate service jatis derive toward symmetry: costs above, gains below. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled. Anti-caste reformers (excluded) and constitutional framers (observer) neither collect nor pay and do not feed the transfer arithmetic. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification guards against two mislabels. Reading the arrangement as rope (pure coordination) would erase the victims: the coordination story — ordering interdependence without centralized administration — is real, but it is the cover under which hereditary transfer operates, and exits are punished rather than merely costly. Reading it as mountain (natural order) is the orthodox seat's own claim — scriptural fixity presented as natural law — and is exactly the false-summit shape the FSM signature exists to catch; here the beneficiaries are declared openly and the claim is snare, so the naturalness claim is carried instead by the karmic-desert and fixity omegas. The R5 interview locates the residual coordination content honestly: the founding problem (ordering a differentiated division of labor without state capacity) is corroborated by religious-historical scholarship from outside the beneficiary set, but its status is contested — modern contract, administration, and voluntary association perform the ordering function without pollution enforcement, while the orthodox seats attest the problem eternal. Founding_problem_status contested paired with disappearance_verdict world_rearranges is the correct signature: the world would rearrange because real coordination content exists, and the dispute over whether the founding problem is live is itself the mandatrophy question. The arrangement is not yet a piton: enforcement, though collapsing at interval end, was load-bearing for most of the interval, and the extraction remains concentrated enough that the beneficiary seats still defend it — but the rising theater series marks the drift path a piton verdict would follow if enforcement decay outpaces extraction decay in the post-abolition era.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_fixity_dispute,
    'This constraint is one reading of the jati_practice_norm kernel. Is the observed boundary rigidity constituted by scriptural fixity with pollution enforcement (this reading), by continuous local renegotiation (localized_practice_reading), or by colonial administrative reification (colonial_census_reading) — and which sibling reading, if adopted, would change the beneficiary/victim structure?',
    'Compare pre-colonial boundary-mobility records (jati status-shift episodes, Sanskritization and secession cases) against colonial-era census stabilization: if boundaries moved substantially before enumeration and froze after, the colonial reading captures the rigidity''s source and part of this reading''s ε is misattributed to scripture.',
    'If the colonial or localized reading is adopted, this constraint''s ε falls (rigidity is manufactured or negotiable rather than ontological), the victim set shrinks toward enforcement episodes, and the classification drifts toward tangled_rope; if this reading is adopted, the siblings understate the enforcement structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_fixity_dispute, conceptual, 'Which sibling reading constitutes the kernel''s actual boundary mechanism — scriptural fixity, local renegotiation, or administrative reification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'After the enforcement machinery''s constitutional dismantling (untouchability abolition, temple-entry acts), does the arrangement''s suppressive force persist because it was internalized (purity norms carried by upper and lower jatis alike, self-policing) or because it remains structural (village segregation, economic dependency, continued private violence)?',
    'Post-enforcement trajectory tracking: compare purity-observance and exclusion rates in regions where enforcement capacity collapsed against regions where economic dependency structures persisted; persistence of exclusion after mechanism removal indicates internalization.',
    'If internalized, effective suppression exceeds the structural measure — the arrangement outlives its enforcement and decays only generationally; if structural, dismantling dependency structures suffices and decay should track enforcement capacity directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression after enforcement decay.').

omega_variable(
    coordination_extraction_separability,
    'Is the occupational and ritual interdependence the framework orders (village service economy, marriage pools, ritual provision) separable from the pollution enforcement that disciplines it, or does the coordination function require boundary rigidity?',
    'Compare service-economy and marriage-coordination persistence in regions and periods where pollution sanctions weakened (temple entry, urban migration, post-reform labor markets) without collapse of service provision or matching.',
    'If separable, the pollution layer is pure transfer riding on a real coordination function; if inseparable, part of measured ε is the price of the coordination itself and the snare/tangled_rope boundary moves.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the coordination and transfer components of the arrangement are structurally separable.').

omega_variable(
    karmic_desert_framing,
    'Within this reading''s own lights, are the burdens assigned to polluting-occupation and servile jatis illicit transfer or deserved allocation under karmic desert — and does the karmic framework''s validity change what this reading counts in ε?',
    'Not resolvable by data: it turns on whether karmic desert is admitted as a valid allocative principle. Resolved only by adopting or rejecting the karmic framework — a values commitment, not an observation.',
    'If desert is admitted, the reading''s own accounting reclassifies most measured transfer as legitimate allocation and ε collapses toward the coordination floor; the authored high ε treats desert claims as the cover story, which the orthodox seat rejects — the disagreement between this story''s ε and the orthodox seat''s self-account is located exactly here.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(karmic_desert_framing, preference, 'Whether karmic-desert framing converts the measured transfer into legitimate allocation within the reading''s own lights.').

omega_variable(
    interpretive_absorption_of_drift,
    'Does the commentarial interpretation layer absorb historical drift — e.g., reinterpreting varna as quality-based (guna) rather than birth-based in modern orthodox apologetics — without surfacing kernel revision, such that the ''fixed'' kernel has already substantially moved?',
    'Trace commentarial and apologetic shifts across the interval (medieval commentary, colonial-era defense, modern orthodox revisionism) against the claimed fixity; systematic silent reinterpretation indicates absorbed drift.',
    'If drift is absorbed, the fixity claim is performatively maintained (a theater signature consistent with the rising theater_ratio series) and the reading''s kernel is fixed_text in name only — the enforcement object has silently become the interpretation layer itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_absorption_of_drift, empirical, 'Whether the interpretation layer silently absorbs kernel drift while fixity is claimed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_orthodox_reading_tr_t0, jati_practice_norm__orthodox_textual_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jati_orthodox_reading_tr_t20, jati_practice_norm__orthodox_textual_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(jati_orthodox_reading_tr_t40, jati_practice_norm__orthodox_textual_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(jati_orthodox_reading_tr_t60, jati_practice_norm__orthodox_textual_reading, theater_ratio, 60, 0.33).
narrative_ontology:measurement(jati_orthodox_reading_tr_t80, jati_practice_norm__orthodox_textual_reading, theater_ratio, 80, 0.42).
narrative_ontology:measurement(jati_orthodox_reading_tr_t100, jati_practice_norm__orthodox_textual_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(jati_orthodox_reading_be_t0, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(jati_orthodox_reading_be_t20, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(jati_orthodox_reading_be_t40, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(jati_orthodox_reading_be_t60, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(jati_orthodox_reading_be_t80, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement(jati_orthodox_reading_be_t100, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jati_orthodox_reading_su_t0, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(jati_orthodox_reading_su_t20, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(jati_orthodox_reading_su_t40, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 40, 0.88).
narrative_ontology:measurement(jati_orthodox_reading_su_t60, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 60, 0.86).
narrative_ontology:measurement(jati_orthodox_reading_su_t80, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 80, 0.68).
narrative_ontology:measurement(jati_orthodox_reading_su_t100, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'the caste system / jati boundaries' decomposes into three sibling readings of the jati_practice_norm kernel — orthodox_textual_reading (this file: boundaries fixed by scripture, deviation as pollution; high ε, snare), localized_practice_reading (boundaries as continuously renegotiated coordination norms; lower ε, rope/tangled_rope candidate), and colonial_census_reading (rigidity manufactured by administrative reification; ε concentrated in the enumeration era). Each is a separate constraint with its own ε, beneficiaries, and victims; none averages across the others. The upstream/downstream structure: this reading supplies the legitimacy vocabulary the census reading reified (hence the influences edge), and it defines as pollution the very renegotiation the localized reading takes as constitutive — no single framework can hold both fixity-as-ontological-boundary and renegotiation-as-norm as core premises (hence the forecloses edge).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
