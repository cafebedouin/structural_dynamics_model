% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__indigenous_stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__indigenous_stewardship_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: cultural_property_legal_corpus__indigenous_stewardship_reading
 *   human_readable: Cultural Property Held Without Indigenous Consent (Indigenous Stewardship Reading)
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint describes the indigenous stewardship reading of the
 *   contested cultural property kernel. Under this reading, cultural
 *   artifacts sacred to or held communally by indigenous communities are
 *   legitimate property of those communities; museums and successor states
 *   holding them without community consent are engaged in ongoing extraction
 *   of cultural authority, knowledge, and spiritual access. The reading is
 *   one of three sibling readings of the same kernel (universal heritage
 *   reading, sovereign repatriation reading); this one instantiates the
 *   indigenous communal-stewardship framing. The constraint is claimed as a
 *   snare because, under this reading, the extraction is pure: the museums
 *   and states defend their custody as beneficial, but the reading rejects
 *   that framing as a cover story for colonial-era appropriation now locked
 *   in by institutional inertia and legal systems inherited from colonialism.
 *   The ε value (0.88) is high because, under this reading, the core
 *   extraction is illegitimate — artifacts are held by parties with no
 *   legitimate claim. The theater ratio (0.41) reflects the substantial gap
 *   between museums' stated mission (preservation, universal access,
 *   scholarship) and the reading's diagnosis (institutional prestige, visitor
 *   revenue, and reproduction of Western expertise hierarchies). Suppression
 *   (0.79) is high because indigenous communities' claim to stewardship is
 *   actively excluded by legal systems, institutional policies, and the
 *   geographic distance that makes reclamation nearly impossible without
 *   foreign governments' consent.
 *
 * KEY AGENTS:
 *   - indigenous_communities: structurally powerless, civilizational time horizon, identity-locked to the artifacts they can no longer access — the reading's primary victim and, paradoxically, rightful beneficiary
 *   - western_museums: institutional power, generational horizon, constrained exit (repatriation threatens endowment and donor expectations) — agenda-setters who enforce the constraint through custody and legal systems
 *   - successor_states: institutional power, generational horizon, constrained exit (competing legitimacy claims) — agenda-setters who enforce legal frameworks that exclude indigenous authority
 *   - descendant_lineage_holders: powerless, civilizational horizon, identity-locked — directly experience the constraint as severed access and broken transmission
 *   - colonial acquisition systems (non-agent): the legal, market, and institutional apparatus that enabled removal and legitimizes continued foreign custody
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88).
domain_priors:suppression_score(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.79).
domain_priors:theater_ratio(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__indigenous_stewardship_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__indigenous_stewardship_reading, snare).
narrative_ontology:human_readable(cultural_property_legal_corpus__indigenous_stewardship_reading, "Cultural Property Held Without Indigenous Consent (Indigenous Stewardship Reading)").
narrative_ontology:topic_domain(cultural_property_legal_corpus__indigenous_stewardship_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__indigenous_stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__indigenous_stewardship_reading, '9264750b-3c45-4d9a-8f1b-4c6951b036bd').
narrative_ontology:cs_kernel_codification('9264750b-3c45-4d9a-8f1b-4c6951b036bd', distributed).
narrative_ontology:cs_authority_grounding('9264750b-3c45-4d9a-8f1b-4c6951b036bd', extraction).
narrative_ontology:cs_interpretation_layer_present('9264750b-3c45-4d9a-8f1b-4c6951b036bd').
narrative_ontology:cs_reading_relation('9264750b-3c45-4d9a-8f1b-4c6951b036bd', cultural_property_legal_corpus__universal_heritage_reading, forecloses).
narrative_ontology:cs_reading_relation('9264750b-3c45-4d9a-8f1b-4c6951b036bd', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_axiom('9264750b-3c45-4d9a-8f1b-4c6951b036bd', foundational, indigenous_cultural_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_cultural_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('9264750b-3c45-4d9a-8f1b-4c6951b036bd', indigenous_cultural_sovereignty, deontological).
narrative_ontology:cs_axiom('9264750b-3c45-4d9a-8f1b-4c6951b036bd', foundational, colonial_acquisition_illegitimacy).
narrative_ontology:cs_axiom_status(colonial_acquisition_illegitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9264750b-3c45-4d9a-8f1b-4c6951b036bd', colonial_acquisition_illegitimacy, deontological).
narrative_ontology:cs_reference_frame('9264750b-3c45-4d9a-8f1b-4c6951b036bd', indigenous_communal_stewardship_authority).
narrative_ontology:cs_drift_state('9264750b-3c45-4d9a-8f1b-4c6951b036bd', contemporary_museum_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9264750b-3c45-4d9a-8f1b-4c6951b036bd', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_lineage_holders).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_cultural_sovereignty).
narrative_ontology:constraint_vindicates(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_acquisition_illegitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold sacred and communal cultural knowledge and artifacts from which their practices, spiritual traditions, and identity derive. Under this reading, they are the legitimate authorities over these artifacts — but they are structurally prevented from exercising that authority because artifacts are held in foreign museums, private collections, and successor-state institutions. They bear the cost of cultural loss, severed transmission, and delegitimized spiritual practice. Exit would require reclamation through legal action (constrained by colonial-era laws), negotiation with unequal power dynamics, or direct action (illegal in host jurisdictions).
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, payer,
    powerless, civilizational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, indigenous_communities, beneficiary).

% Hold physical custody and legal title to artifacts acquired during the colonial period and after. Frame their role as preservation, research, and universal access — functions they argue justify custody regardless of origin. Under this reading, they are extractors: they control artifacts that do not legitimately belong to them, benefit from scholarly prestige and visitor revenue, and enforce their custody through legal systems inherited from colonialism. Their exit option (repatriation) is constrained by institutional identity, endowment dependency, and donor expectations.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, western_museums, agenda_setter,
    institutional, generational, constrained, global).

% Claim sovereignty over cultural property within their borders and sometimes over ancestral artifacts held abroad, arguing they represent continuity with precolonial peoples. Under this reading, they are extractors when they treat artifacts as state property rather than recognizing indigenous community authority. They enforce legal frameworks that exclude indigenous communities from decision-making and benefit from cultural nationalism that valorizes artifacts as national heritage. Their constraint derives from competing legitimacy claims — both state and indigenous community claim continuity — but states have institutional power to enforce their claim.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__indigenous_stewardship_reading, successor_states, payer).

% The legal, market, and institutional structures that enabled colonial-era artifact removal and continue to legitimize foreign custody. Not an actor but an operative system that structures who can hold, sell, and claim authority over artifacts.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_acquisition_systems, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(cultural_property_legal_corpus__indigenous_stewardship_reading, colonial_acquisition_systems).

% The currently dominant doctrinal framework (UNESCO conventions, UNDRIP, national repatriation laws) treats artifacts as either state property or universal heritage, not indigenous communal property. Under this reading, the framework itself is the enforcement mechanism for the extraction — it provides the legal legitimacy for excluding indigenous authority.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, international_law_framework, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(cultural_property_legal_corpus__indigenous_stewardship_reading, international_law_framework).

% Descendants of the original communities, often living in the same regions where artifacts originated. They carry the spiritual and practical knowledge encoded in artifacts but lack legal standing to claim them. Their identity is constituted through relationship to the artifacts; exit from that identity is not possible without ceasing to be members of their communities.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, descendant_lineage_holders, beneficiary,
    powerless, civilizational, identity_locked, regional).

% Were the artifacts' original holders; their voices are historicized and silenced by the time artifact debates occur in modern legal forums. Structurally excluded from contemporary decisions about artifacts that embody their cultural continuity. Inclusion would fundamentally reframe the legitimacy question.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__indigenous_stewardship_reading, colonized_peoples_historical, excluded,
    powerless, civilizational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__indigenous_stewardship_reading, western_museums).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__indigenous_stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stewardship and transmission of sacred and communal artifacts within indigenous communities — cultural knowledge, spiritual practice, and identity continuity depend on communities' unmediated access to and authority over these objects. Under this reading, the constraint (if reframed as indigenous authority) would solve the coordination problem of preserving artifacts according to their communities' own values and transmission protocols rather than colonial-era conservation models.
% TRANSFER_FUNCTION: Moves authority, custody, and benefit from indigenous communities (legitimate holders under this reading) to western museums and successor states (current extractive holders). The extraction includes: physical control of sacred objects, scholarly prestige and publication rights, visitor revenue and cultural soft power, and the institutional reproduction of expert/layperson hierarchies that treat indigenous knowledge as artifact rather than living tradition.
% ABSENT_VOICES: Descendant lineage holders with direct knowledge of artifact use and meaning are largely absent from the international law forums where cultural property disputes are adjudicated. Communities living far from museums cannot easily testify about their cultural needs. Oral traditions and spiritual practitioners are excluded by evidentiary and disciplinary frameworks that privilege written records and Western scholarship. Their inclusion would demand recognizing artifacts as still-living cultural property, not historical objects.
% DISAPPEARANCE_RATIONALE: If this constraint (indigenous stewardship authority) were fully operative, artifacts would return to community custody, transmission practices would restart in their original contexts, and the museum/state control apparatus would lose physical custody and legitimacy. The global art market, scholarly knowledge production, and institutional prestige structures built on artifact custody would reorganize. This is the constraint's essential claim: its disappearance is not a return to a prior state but a transformation into indigenous-centered stewardship.
% FOUNDING_PROBLEM: Indigenous communities' sacred and communal artifacts were removed during colonialism by force, trade, or coercion. The removal violated the communities' authority and broke transmission lines. Communities lost access to objects essential to spiritual practice, identity, and knowledge transmission. The founding problem is the original dispossession and the continuing structural exclusion from authority over what belongs to them.
% FOUNDING_PROBLEM_CORROBORATION: Attested by indigenous community leaders, descendant lineage holders, and independent scholars of colonialism and cultural anthropology (Karp, Kratz, Lavine on museum politics; Todd on indigenous data sovereignty; Simpson on repatriation resistance). Independent corroboration comes from UN bodies (UNDRIP Article 31 on indigenous intellectual property), repatriation case law testimony, and oral history projects documenting cultural rupture. The founding problem is NOT attested by the museums and states that currently hold artifacts — their narrative is that acquisition was legitimate and custody is beneficial.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__indigenous_stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__indigenous_stewardship_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__indigenous_stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__indigenous_stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__indigenous_stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.88) because the reading treats the core situation as illegitimate: artifacts belong to indigenous communities under principles of cultural sovereignty and communal property, but they are held by parties (museums, states) with no valid claim under this framework. The extraction includes not just physical custody but also the institutional prestige, scholarly authority, and narrative control that flows from holding the artifacts. Suppression is nearly as high (0.79) because indigenous communities' claim to stewardship is excluded by multiple reinforcing structures: legal systems inherited from colonialism, institutional policies that treat artifacts as Western property, the geographic distance and resource asymmetry that makes reclamation nearly impossible without foreign government cooperation, and the internalized deference of many communities to Western expertise. Theater ratio is moderate (0.41) because museums perform significant curatorial and preservation work, but the reading diagnosis is that preservation is no longer the primary function — instead, the institutions defend their custody as beneficial to justify what is fundamentally an extraction of cultural authority from communities with legitimate claim. The measurement series show extractiveness rising from 0.82 to 0.88 over 40 years, reflecting increasing institutional entrenchment (newer museums built, acquisition justified through cultural nationalism, legal frameworks codified). Theater ratio rises slightly (0.32 to 0.41) as museums invest more heavily in community engagement narratives without transferring authority. Suppression requirement stabilizes high (0.75 to 0.79) because preventing indigenous community stewardship requires continuous legal enforcement and institutional gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The engine computes this gap from the structural data. From the indigenous community perspective: this is pure extraction, a continuation of colonial dispossession, with no coordination function and no legitimate claim by the current holders. From the museum perspective: this is coordination (they solve problems of preservation and scholarly access) that also generates institutional benefit — they would characterize the constraint as rope or tangled rope. From the successor state perspective: this is either rope (they coordinate national cultural identity) or snare depending on how much weight they give to indigenous vs. state claims. The gap is not about measurement disagreement but about fundamentally different legitimacy frameworks — different readings of the same kernel. The engine's job is to compute per-seat type from the structural data; the divergence signals that the reading choice (which legitimacy framework to adopt) is doing the classificatory work.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges sharply across seats. Indigenous communities approach d = 1.0 (full targets of extraction): they bear the cost of severed transmission, delegitimized spiritual practice, and exclusion from decision-making, while receiving no benefit from the constraint. Museums and states approach d near beneficiary end (though not <0.0, since they do perform preservation): they set the rules, collect prestige and revenue, and face no cost other than the institutional friction of defending their position. The power asymmetry (museums and states are institutional; indigenous communities are powerless) amplifies the directional difference: exit for indigenous communities is trapped or identity_locked, making them fully exposed to extraction; exit for museums is constrained but available (repatriation is a real option, though institutionally painful). This structural asymmetry is the core of the snare diagnosis: one seat cannot exit, the other can but chooses not to, and the rule persists through enforcement rather than coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   Under this reading, the founding problem is live: indigenous communities have not regained stewardship authority, artifacts remain separated from communities, and transmission lines remain broken. The constraint's founding mandate (to preserve artifacts and provide access) was legitimate when understood as temporary colonial-era curation pending return to communities; if that return never occurs, the mandate becomes zombified — the institution persists to justify its own existence rather than to solve the founding problem. However, the measurement series do not show the constraint decaying into pure theater (theater_ratio plateaus at 0.41, not rising toward 0.7+); museums are still performing genuine preservation work. The mandatrophy is partial: the preservation function persists but is now embedded in an extraction apparatus that prevents its own resolution (return of artifacts). The constraint would be resolved (extraction ended) not by the institution's collapse but by its restructuring: repatriation, transfer of decision-making authority to communities, and shift from Western-expert-centric stewardship to community-directed transmission protocols.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_contest,
    'Is this the indigenous stewardship reading, or one of the sibling readings (universal heritage, sovereign repatriation)? Which reading''s framing of legitimate authority is structurally true?',
    'The three readings form an irreducible kernel contest — no empirical discovery resolves which is correct because they root in different legitimacy postulates (indigenous communal authority vs. state sovereignty vs. universal humanity). Resolution is political/normative, not empirical.',
    'The ε value (0.88) describes THIS reading''s assessment that artifacts held by museums and states are held without legitimate claim. A sibling reading would author the same artifacts differently — as legitimately held under state sovereignty (sovereign repatriation reading, lower ε) or as universally stewarded (universal heritage reading, lowest ε). The classification hinges on which reading''s legitimacy framework is adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_contest, conceptual, 'Which reading of the cultural property kernel is structurally true — indigenous stewardship, state sovereignty, or universal heritage?').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.79) primarily structural (legal systems, geographic distance, institutional barriers that would collapse if removed) or internalized (indigenous communities internalize the museum/state narrative of ''proper stewardship'' such that suppression persists even if barriers are lifted)?',
    'Post-repatriation trajectory: if communities quickly resume traditional transmission practices and confidence in self-stewardship after artifacts return, suppression was primarily structural. If hesitation, deference to expert judgment, or internalized doubt persists, suppression is partially internalized and the communities carry the constraint with them.',
    'If primarily structural, repatriation and legal authority transfer resolve the extraction. If partially internalized, the constraint''s effective suppression is higher than institutional measures alone suggest — resolution requires also restoring cultural confidence and de-legitimizing the expertise hierarchy that positioned indigenous knowledge as mere ''artifact.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of indigenous stewardship authority is structural or internalized.').

omega_variable(
    identity_lock_civilization_scale,
    'For descendant lineage holders with identity constituted through relationship to artifacts, what is the relationship between cultural identity and exit? Can someone remain a community member without access to the artifacts, or is the artifact access prerequisite to the identity itself?',
    'Ethnographic study of communities'' own accounts of identity, loss, and what repatriation would restore. Internal debate within communities about transmission without physical artifacts (digital archives, oral transmission) vs. insistence on physical return.',
    'If artifacts are identity-constitutive (cannot be a full community member without them), the exit_options classification for descendant lineage holders is correctly ''identity_locked'' at civilizational horizon. The constraint then extracts not just access but identity itself. If identity is separable from artifact access (communities maintain identity through other means), exit is ''constrained'' rather than ''identity_locked'' and the extraction measure should be lowered.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_civilization_scale, empirical, 'Whether indigenous identity is constituted through artifact access or separable from it.').

omega_variable(
    measurement_grid_cyclical_dynamics,
    'Why do the measurement values show a plateau from t=32 onward (extractiveness and theater_ratio cease rising)? Is this a genuine stabilization of the constraint or a limitation of the measurement window?',
    'Extended timeline (50+ years) to determine whether the constraint reaches a true equilibrium or continues slow rise beyond the measurement interval. Examine whether repatriation victories (Benin Bronzes, Parthenon marbles, etc.) are reflected in the series as local dips (not visible in this series) or whether artifact return has no measurable effect on the aggregate constraint.',
    'If true equilibrium: the extraction intensity plateaus at 0.88 and the theater ratio stabilizes at 0.41, suggesting the constraint has hardened into a stable institutional form. If continuation beyond the window: the plateau is an artifact of short-term measurement and the constraint is still tightening.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurement_grid_cyclical_dynamics, empirical, 'Whether the constraint''s extraction and theater ratio have stabilized or continue rising beyond the measurement interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__indigenous_stewardship_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(cult_tr_t0, observed).
narrative_ontology:measurement(cult_tr_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement_basis(cult_tr_t8, observed).
narrative_ontology:measurement(cult_tr_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement_basis(cult_tr_t16, observed).
narrative_ontology:measurement(cult_tr_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(cult_tr_t24, observed).
narrative_ontology:measurement(cult_tr_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(cult_tr_t32, observed).
narrative_ontology:measurement(cult_tr_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(cult_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 0, 0.82).
narrative_ontology:measurement_basis(cult_be_t0, observed).
narrative_ontology:measurement(cult_be_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 8, 0.84).
narrative_ontology:measurement_basis(cult_be_t8, observed).
narrative_ontology:measurement(cult_be_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 16, 0.86).
narrative_ontology:measurement_basis(cult_be_t16, observed).
narrative_ontology:measurement(cult_be_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 24, 0.87).
narrative_ontology:measurement_basis(cult_be_t24, observed).
narrative_ontology:measurement(cult_be_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 32, 0.88).
narrative_ontology:measurement_basis(cult_be_t32, observed).
narrative_ontology:measurement(cult_be_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, base_extractiveness, 40, 0.88).
narrative_ontology:measurement_basis(cult_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement_basis(cult_su_t0, observed).
narrative_ontology:measurement(cult_su_t8, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 8, 0.76).
narrative_ontology:measurement_basis(cult_su_t8, observed).
narrative_ontology:measurement(cult_su_t16, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 16, 0.77).
narrative_ontology:measurement_basis(cult_su_t16, observed).
narrative_ontology:measurement(cult_su_t24, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 24, 0.78).
narrative_ontology:measurement_basis(cult_su_t24, observed).
narrative_ontology:measurement(cult_su_t32, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 32, 0.79).
narrative_ontology:measurement_basis(cult_su_t32, observed).
narrative_ontology:measurement(cult_su_t40, cultural_property_legal_corpus__indigenous_stewardship_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(cult_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__indigenous_stewardship_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__indigenous_stewardship_reading, 0.12).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__universal_heritage_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__indigenous_stewardship_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).

% DUAL FORMULATION NOTE:
% The cultural_property_legal_corpus kernel generates three distinct constraint stories, one per reading of legitimate cultural property authority. Each reading instantiates a different ε, beneficiary/victim structure, and classification. The indigenous_stewardship_reading (this story) treats artifacts held by museums and states as extracted from their legitimate community holders (ε=0.88). The universal_heritage_reading treats the same artifacts as legitimately stewarded by institutions for universal benefit (lower ε). The sovereign_repatriation_reading treats them as extracted by colonial powers but legitimately held by successor states (intermediate ε). The three readings are linked via network.affects_constraints to indicate kernel membership and mutual influence. Decomposition per ε-invariance: the observable is the same (where are the artifacts, who makes decisions), but the referent changes with the reading's legitimacy framework (what is the artifact under indigenous law vs. international heritage law vs. successor state law). Each story authors its own ε relative to its own reading's framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, powerless, 1.0).
constraint_indexing:directionality_override(cultural_property_legal_corpus__indigenous_stewardship_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
