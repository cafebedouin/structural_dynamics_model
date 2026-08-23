% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Timeless Covenant Separation Mandate (Durable Separation Reading)
 *   domain: biblical hermeneutics / religious ethics / commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the herem kernel (Deuteronomy 7):
 *   the durable separation reading, which holds the mandate — destroy the
 *   designated nations, do not intermarry with them, for they will turn your
 *   children away — as a timeless divine statute governing covenant identity
 *   in every generation. Under this reading the mandate is fully operative:
 *   every cost the text specifies is live. The epsilon referent is the
 *   standing arrangement under contest — the mandate as codified and as held
 *   operative by communities adopting this reading — assessed by the
 *   reading's own lights. The reading ENDORSES the arrangement; endorsement
 *   explains why it persists, not why epsilon is low: the costs it imposes on
 *   governed parties (intermarriage autonomy, outsider legitimacy, member
 *   enforcement burden) are structural facts the reading itself acknowledges,
 *   framing them as covenant duty and divine justice. Because this reading
 *   makes the mandate maximally binding, it carries the highest extraction
 *   profile of the three sibling readings. The siblings are separate
 *   constraints, not hedges inside this one (DP-001 epsilon-invariance): the
 *   contextual supersession reading bounds the mandate historically and
 *   shrinks the victim set to ancient actors; the allegorical displacement
 *   reading replaces real-group referents with typological vices and empties
 *   the victim set entirely. Both are linked via network.affects_constraints.
 *   The claimed_type (tangled_rope) and the metrics are independently
 *   authored facts: the claim states my structural belief that the
 *   arrangement genuinely coordinates identity persistence while extracting
 *   asymmetrically through the same structure; the metrics describe its
 *   observed operation.
 *
 * KEY AGENTS:
 *   - - religious_authority_class: Agenda-setter (institutional/identity_locked) — interprets, administers, and enforces the boundary; collects authority, standing, and livelihood from the mandate's continued binding force
 *   - - covenant_community_core: Primary beneficiary with payer exposure (organized/identity_locked) — receives identity security, trust density, and mutual aid; pays foregone relational autonomy and the burden of policing its own attachments
 *   - - non_covenant_outsiders: Primary target (powerless/trapped) — categorized as contamination threat by a mandate they did not author; bear exclusion, suspicion, and enforcement-episode exposure
 *   - - would_be_intermarrying_couples: Target (moderate/trapped) — their cross-boundary attachments are the direct object the constraint extracts from
 *   - - internal_interpretive_dissenters: Excluded voice with payer exposure (moderate/identity_locked) — their alternative readings are pre-classified as disobedience rather than interpretation; bear sanction when they persist
 *   - - hermeneutical_scholars: Analytical observer (analytical/analytical) — sees the three-reading structure from outside the covenant frame; collects nothing from the mandate's operation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.74).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Covenant Separation Mandate (Durable Separation Reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical hermeneutics / religious ethics / commitment systems").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '3a151166-7375-48cb-aaa8-76177bc9c7f8').
narrative_ontology:cs_kernel_codification('3a151166-7375-48cb-aaa8-76177bc9c7f8', fixed_text).
narrative_ontology:cs_authority_grounding('3a151166-7375-48cb-aaa8-76177bc9c7f8', extraction).
narrative_ontology:cs_interpretation_layer_present('3a151166-7375-48cb-aaa8-76177bc9c7f8').
narrative_ontology:cs_reading_relation('3a151166-7375-48cb-aaa8-76177bc9c7f8', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('3a151166-7375-48cb-aaa8-76177bc9c7f8', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('3a151166-7375-48cb-aaa8-76177bc9c7f8', foundational, mandate_temporally_unbounded).
narrative_ontology:cs_axiom_status(mandate_temporally_unbounded, holdable).
narrative_ontology:cs_axiom_grounding('3a151166-7375-48cb-aaa8-76177bc9c7f8', mandate_temporally_unbounded, theological).
narrative_ontology:cs_axiom('3a151166-7375-48cb-aaa8-76177bc9c7f8', foundational, separation_constitutive_of_covenant_identity).
narrative_ontology:cs_axiom_status(separation_constitutive_of_covenant_identity, holdable).
narrative_ontology:cs_axiom_grounding('3a151166-7375-48cb-aaa8-76177bc9c7f8', separation_constitutive_of_covenant_identity, theological).
narrative_ontology:cs_axiom('3a151166-7375-48cb-aaa8-76177bc9c7f8', secondary, obedience_legitimates_boundary_enforcement).
narrative_ontology:cs_axiom_status(obedience_legitimates_boundary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('3a151166-7375-48cb-aaa8-76177bc9c7f8', obedience_legitimates_boundary_enforcement, theological).
narrative_ontology:cs_reference_frame('3a151166-7375-48cb-aaa8-76177bc9c7f8', standing_divine_boundary_mandate).
narrative_ontology:cs_drift_state('3a151166-7375-48cb-aaa8-76177bc9c7f8', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a151166-7375-48cb-aaa8-76177bc9c7f8', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, religious_authority_class).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_core).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsiders).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, would_be_intermarrying_couples).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, internal_interpretive_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, covenant_community_core).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, covenant_election_distinctness).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, separation_constitutes_identity).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_command_timelessness).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the text, decides who counts as a designated outsider, adjudicates violations, and performs the separation discipline. Standing, livelihood, and adjudication authority flow from the mandate's continued binding force; teaching its timelessness is the office's core activity. Leaving the system would dissolve the authority the office consists in, so exit is not a practical option from inside the role.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, religious_authority_class, agenda_setter,
    institutional, generational, identity_locked, global).

% Receives identity security, dense trust networks, mutual aid, and cultural continuity from bounded membership sustained across generations and dispersion. Pays in foregone marriage and association autonomy, in the labor of policing its own attachments, and in hostility the separation stance generates from surrounding societies. Departure would mean losing the entire social world at once, so staying is experienced less as a choice than as being who one is.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_core, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_core, payer).

% Are categorized as contamination threats by a mandate they did not author and have no seat in interpreting. Bear exclusion, suspicion, and — in enforcement episodes — exposure to sanctioned expulsion or worse, legitimated by citation of the command. The only exit the system offers is full assimilation-conversion on the insider's terms, which validates the category by submitting to it.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsiders, payer,
    powerless, biographical, trapped, global).

% Mixed attachments form spontaneously across the boundary; the mandate converts each one into a crisis with three doors — dissolve the relationship, conceal it, or leave the community. Their preference for each other is the direct object the constraint acts on, and every available door costs them something irreplaceable.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, would_be_intermarrying_couples, payer,
    moderate, biographical, trapped, global).

% Members who read the mandate as historically bounded or question its timelessness. Their readings are pre-classified as disobedience rather than engaged as interpretation, which keeps them out of the authoritative conversation despite their presence inside the community. Those who persist publicly bear sanction, marginalization, or excommunication; privately they carry the membership they cannot comfortably hold.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, internal_interpretive_dissenters, excluded,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, internal_interpretive_dissenters, payer).

% Study the text, its reception history, and the three-reading contest from outside the covenant frame. Document how each reading distributes costs and which populations each leaves with live obligations. Hold no enforcement power, collect nothing from the mandate's operation, and can see the full structure including the seats the frame itself cannot represent.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, hermeneutical_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, religious_authority_class).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains bounded group identity across generations and geographic dispersion: shared membership criteria, intra-group trust, mutual aid, and cultural transmission are secured by categorical separation rules enforced communally, solving the collective-action problem of a minority identity surviving inside absorptive host societies.
% TRANSFER_FUNCTION: Moves relational freedom and associational legitimacy from community members (marriage and friendship across the boundary) and existential recognition from outsiders (their standing as legitimate neighbors) toward the covenant identity structure and the authority class that administers it.
% ABSENT_VOICES: The designated outsiders themselves have no seat anywhere in the interpretive tradition that defines them as threats — the people the mandate categorizes are precisely the people the categorization excludes from answering it. Internal dissenters who read the mandate as bounded are present in the community but excluded from the authoritative conversation, their objections pre-classified as sin. Both absences are structural: the mandate's authority form (divine command) does not admit challenge from its objects.
% DISAPPEARANCE_RATIONALE: Communities organized around this reading would lose the boundary that constitutes them: intermarriage patterns would open within a generation, the authority class's adjudication monopoly would dissolve, diaspora identity structures built on endogamy would reorganize around voluntary affiliation, and the enforcement machinery (sanction, excommunication, family rupture) would lose its object. The surrounding societies' relationship to the community would also rearrange, since the separation stance shapes both sides of the boundary.
% FOUNDING_PROBLEM: A small settlement-era polity surrounded by larger, culturally absorptive powers faced existential assimilation pressure: intermarriage and cultic blending threatened the discontinuity of the covenant community, a crisis renewed acutely in the post-exilic period when documented intermarriage rates prompted forced dissolution of mixed households.
% FOUNDING_PROBLEM_CORROBORATION: Historical-critical scholarship — outside the beneficiary set — corroborates the original founding problem as real and context-bound: the settlement and post-exilic assimilation pressures are documented. Sociology of religion corroborates that identity persistence across dispersion is achievable through mechanisms other than categorical separation. No source outside the benefiting parties attests that the founding problem remains live today: the liveness claim is attested only from within the covenant frame, and that corroboration asymmetry is itself the signal.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.78, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the mandate, taken as timeless, prices three things continuously: insiders' marriage and association autonomy, outsiders' standing as legitimate neighbors, and the community's enforcement labor. Suppression (0.74) is a raw structural property, unscaled by power or scope: persistence depends on active machinery — communal sanction, excommunication, family rupture for violators — not on voluntary preference. Theater is moderate-low (0.36): the boundary function is real and load-bearing, but a visible share of activity is ritualized display of separation whose informational content is thin. Accessibility_collapse (0.78) is high: once the reading is embraced, integration, intermarriage, and ecumenical partnership collapse from options into sins; the remaining exit is apostasy, priced as total social death. Resistance (0.55) is substantial and documented: the ban was violated persistently across the whole record (the post-exilic crisis exists precisely because intermarriage was rampant), and modern reform movements reject the reading outright — yet within holding communities resistance is criminalized as disobedience, which caps its expression. Coalition check: non_covenant_outsiders as a class could in principle coordinate, but the mandate's categorization architecture is designed to prevent exactly that cross-boundary coalition — blocking it is part of the extraction mechanism, not an accident. Identity-lock dynamics: the authority class exhibits professional/institutional fusion (the office IS the boundary administration; exit dissolves the office); the covenant core exhibits relational and ideological fusion (self-concept constituted through membership; exit experienced as self-annihilation); if the identity frame broke, the insider seats would recompute toward mobile exit and the enforcement requirement would collapse. Cyclical pattern: the measurement series runs on one shared time grid (t=0 is the Deuteronomically coded mandate, roughly the seventh-century-BCE codification; t=2648 is the present) and shows roughly two and a half full cycles of crisis-driven intensification and relaxation — post-exilic enforcement peak (forced dissolution of mixed households), rabbinic domestication of herem-war into excommunication, medieval hardening under external persecution, emancipation-era enforcement collapse, and contemporary hardline resurgence. The oscillation is not noise: each crisis re-legitimates the mandate ('this is why separation is necessary'), an intermittent-reinforcement dynamic in which the cycle itself is part of the persistence mechanism. Base_properties values are the interval-end state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and the divergence is the data. From the agenda-setter seat the arrangement is a sacred trust it administers: the mandate is not experienced as extraction but as stewardship of identity, and its costs are reframed as the price of fidelity. From the covenant-core seat the same structure is a mixed ledger — genuine belonging and mutual aid received, autonomy and enforcement burden paid, with the payment experienced as duty rather than loss (the internalization omega governs how much of that experience is fused identity versus accurate accounting). From the outsider seat there is no mixed ledger at all: the constraint assigns them a category — contamination threat — they cannot argue with, exit only by validating the category through assimilation on the insider's terms, and bear enforcement-episode exposure without representation. From the dissenting-insider seat the constraint appears as an interpretive monopoly: their alternative readings are not engaged but pre-classified as sin, which is why they sit in the excluded role despite being inside the community. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations map to directionality as follows. religious_authority_class (institutional, identity_locked) sits near the beneficiary pole: it collects authority and livelihood from the mandate's operation and bears almost none of its costs. covenant_community_core declares as beneficiary but carries real payer exposure — foregone marriage autonomy, enforcement labor, and crisis-generated hostility — so the pure beneficiary derivation would understate its target side; the directionality override (organized power, d=0.35) places it mid-low: net beneficiary, materially targeted on the autonomy dimension. non_covenant_outsiders (powerless, trapped) sit near the full-target pole: the constraint's entire categorizing force lands on them and their exit validates the category. would_be_intermarrying_couples (moderate, trapped) are the most direct targets — the thing extracted is precisely their attachment. internal_interpretive_dissenters (moderate, identity_locked) are targets on the interpretive dimension: the constraint extracts their standing as legitimate readers. hermeneutical_scholars are analytical and directionality-neutral. Larger spatial scope (global, via diaspora) amplifies effective extraction for the target seats by making verification and escape harder; the engine owns that arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real: a small settlement-era polity surrounded by larger culturally absorptive powers faced genuine assimilation pressure, and the post-exilic community renewed the mandate under documented intermarriage rates that threatened discontinuity. Its status today is contested: the benefiting parties attest the assimilation threat is perennial and live; historical-critical scholarship corroborates the original problem as real but context-bound, and the sociology of religion corroborates that identity persistence is achievable through mechanisms other than categorical separation. No source outside the beneficiary set attests liveness — that asymmetry is itself signal. The classification work here prevents two mislabels. Against rope: the arrangement does solve a real coordination problem (identity persistence, trust density, mutual aid under dispersion), but the same structure extracts existentially from outsiders and materially from insiders' autonomy, with concentrated capture by the authority class — that is tangled_rope's signature, not pure coordination. Against snare: the coordination story is not cover — insiders verifiably receive what the story promises, and the arrangement would not survive if the insider benefit were fictitious. Against piton: enforcement is active, not inertial, and capture is concentrated (gain_flow names a seat), so neither the cost-asymmetry nor the no-beneficiary test for piton obtains. The R5 mismatch consumer reads founding_problem_status (contested) x disappearance_verdict (world_rearranges): no dead-problem zombie flag fires, but the corroboration asymmetry is flagged for review.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is ONE reading of kernel herem_command_dt7 (the durable separation reading). What temporal scope and referent do the sibling readings (contextual_supersession_reading, allegorical_displacement_reading) assign to the same text, and how would the victim set and epsilon change under each?',
    'Comparative classification of the sibling stories: observe which populations each reading leaves with live obligations, and track adoption patterns across interpretive communities over time.',
    'Under the supersession reading the victim set collapses to historical actors and epsilon falls toward rope range; under the allegorical reading real-group victims vanish entirely and the constraint becomes an inward discipline. The disagreement is located in two structural elements: the mandate''s temporal scope (timeless vs. bounded) and the referent of ''the nations'' (real groups vs. typological placeholders).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame routing: one kernel, three readings, structurally distinct constraints.').

omega_variable(
    destruction_clause_latency,
    'Does the reading''s retention of the destruction clause constitute live violence legitimation in enforcing communities, or a dormant textual inheritance domesticated into excommunication and social ban?',
    'Examine enforcement episodes in communities holding this reading: is severe action against designated outsiders justified by direct citation of the mandate itself, or only by generic communal-sanction language?',
    'Live legitimation pushes affected seat classifications toward snare; demonstrated dormancy supports the tangled_rope claim authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(destruction_clause_latency, empirical, 'Whether the violence-legitimating component of the mandate is operative or latent.').

omega_variable(
    insider_net_position,
    'Are covenant community members net beneficiaries once foregone relational autonomy, enforcement burden, and externally generated hostility attributable to the separation stance are counted against identity security, trust density, and mutual aid?',
    'Longitudinal welfare and cohesion comparison between strictly separating communities and demographically comparable integrating communities sharing the same external environment.',
    'A net-payer finding thins the coordination story and shifts classification toward snare; a net-beneficiary finding confirms the tangled_rope structure (coordinated insiders, catastrophically paying outsiders).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insider_net_position, empirical, 'Net position of the insider seat under full cost accounting.').

omega_variable(
    suppression_internalization_split,
    'Is the measured suppression structural (communal sanction, excommunication, family rupture) or internalized (members experience cross-boundary attachment as sin or temptation, carrying the prohibition inside the self)?',
    'Post-exit suppression trajectory: members who leave the community and subsequently form cross-boundary attachments without residual distress indicate structural suppression; persistent guilt, avoidance, and identity disruption indicate internalized carryover.',
    'If substantially internalized, effective suppression exceeds the structural measure and the identity_lock on the insider seats deepens; exit becomes psychologically unavailable even where socially possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_split, empirical, 'Structural vs. internalized suppression mechanism split.').

omega_variable(
    authority_grounding_framing,
    'Is the reading''s authority structure grounded in lineage (a Sinai-to-sages transmission chain conferring interpretive authority) or in extraction (benefit accrued from preventing kernel revision, the timelessness claim functioning as drift denial)?',
    'Test which framing better predicts the authority structure''s response to revision proposals: a lineage framing predicts appeals to chain-of-tradition credentials; an extraction framing predicts escalating stakes deployed against revisers irrespective of their traditional credentials.',
    'A lineage framing yields a conventional-authority commitment-system pattern with softer foreclosure computation; the extraction framing authored here treats drift denial as the authority''s revenue stream and sharpens the per-seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Alternative coherent framings of the same authority structure producing different CS classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 2648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(here_tr_t200, herem_command_dt7__durable_separation_reading, theater_ratio, 200, 0.12).
narrative_ontology:measurement(here_tr_t650, herem_command_dt7__durable_separation_reading, theater_ratio, 650, 0.3).
narrative_ontology:measurement(here_tr_t1300, herem_command_dt7__durable_separation_reading, theater_ratio, 1300, 0.34).
narrative_ontology:measurement(here_tr_t1900, herem_command_dt7__durable_separation_reading, theater_ratio, 1900, 0.31).
narrative_ontology:measurement(here_tr_t2350, herem_command_dt7__durable_separation_reading, theater_ratio, 2350, 0.38).
narrative_ontology:measurement(here_tr_t2550, herem_command_dt7__durable_separation_reading, theater_ratio, 2550, 0.47).
narrative_ontology:measurement(here_tr_t2600, herem_command_dt7__durable_separation_reading, theater_ratio, 2600, 0.43).
narrative_ontology:measurement(here_tr_t2648, herem_command_dt7__durable_separation_reading, theater_ratio, 2648, 0.36).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(here_be_t200, herem_command_dt7__durable_separation_reading, base_extractiveness, 200, 0.84).
narrative_ontology:measurement(here_be_t650, herem_command_dt7__durable_separation_reading, base_extractiveness, 650, 0.71).
narrative_ontology:measurement(here_be_t1300, herem_command_dt7__durable_separation_reading, base_extractiveness, 1300, 0.67).
narrative_ontology:measurement(here_be_t1900, herem_command_dt7__durable_separation_reading, base_extractiveness, 1900, 0.77).
narrative_ontology:measurement(here_be_t2350, herem_command_dt7__durable_separation_reading, base_extractiveness, 2350, 0.75).
narrative_ontology:measurement(here_be_t2550, herem_command_dt7__durable_separation_reading, base_extractiveness, 2550, 0.63).
narrative_ontology:measurement(here_be_t2600, herem_command_dt7__durable_separation_reading, base_extractiveness, 2600, 0.7).
narrative_ontology:measurement(here_be_t2648, herem_command_dt7__durable_separation_reading, base_extractiveness, 2648, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(here_su_t200, herem_command_dt7__durable_separation_reading, suppression_requirement, 200, 0.88).
narrative_ontology:measurement(here_su_t650, herem_command_dt7__durable_separation_reading, suppression_requirement, 650, 0.66).
narrative_ontology:measurement(here_su_t1300, herem_command_dt7__durable_separation_reading, suppression_requirement, 1300, 0.61).
narrative_ontology:measurement(here_su_t1900, herem_command_dt7__durable_separation_reading, suppression_requirement, 1900, 0.79).
narrative_ontology:measurement(here_su_t2350, herem_command_dt7__durable_separation_reading, suppression_requirement, 2350, 0.77).
narrative_ontology:measurement(here_su_t2550, herem_command_dt7__durable_separation_reading, suppression_requirement, 2550, 0.5).
narrative_ontology:measurement(here_su_t2600, herem_command_dt7__durable_separation_reading, suppression_requirement, 2600, 0.6).
narrative_ontology:measurement(here_su_t2648, herem_command_dt7__durable_separation_reading, suppression_requirement, 2648, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the herem command' decomposes into three structurally distinct claims per the epsilon-invariance principle. The codified text is the shared kernel; the readings differ on temporal scope of binding force and referent of 'the nations,' producing widely separated epsilon values and disjoint victim sets. This story (durable_separation_reading) is the downstream-most and most extractive member: it cites the text's full force as presently binding, and the other two readings are intelligible largely as responses to the extraction and violence-legitimation problems this reading generates. Each file links the other two via network.affects_constraints; no single story hedges across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(herem_command_dt7__durable_separation_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
