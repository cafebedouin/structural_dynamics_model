% ============================================================================
% CONSTRAINT STORY: cultural_property_legal_corpus__universal_heritage_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cultural_property_legal_corpus__universal_heritage_reading, []).

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
 *   constraint_id: cultural_property_legal_corpus__universal_heritage_reading
 *   human_readable: Universal Heritage Reading of Cultural Property Law
 *   domain: international_law/cultural_property/post_colonial_studies
 *
 * SUMMARY:
 *   This constraint instantiates the UNIVERSAL HERITAGE reading of the
 *   cultural property legal corpus — one of three competing frameworks for
 *   legitimate authority over cultural artifacts. Under this reading,
 *   cultural artifacts are framed as humanity's shared heritage; the
 *   institutions that maximize preservation and provide universal
 *   scholarly/tourist access hold legitimate stewardship authority regardless
 *   of geographic origin or prior possession. Holding institutions (primarily
 *   Western museums and archives) benefit from this framing as it justifies
 *   retention. Successor states claiming repatriation and indigenous
 *   communities claiming sacred stewardship are treated as particularist and
 *   therefore secondary to the universal good. The constraint is CLAIMED as
 *   tangled_rope (coordination + extraction) because holding institutions
 *   frame it that way: genuine preservation coordination justifying
 *   extraction from claimants. The authored metrics describe high extraction
 *   (0.78), high suppression (0.71), and rising theater (0.42) — the payers
 *   and excluded seats would compute this as snare. The measurement grid
 *   shows suppression rising at organizational level (state-to-state
 *   resistance) and individual level (community resistance) while
 *   structural-level suppression rises more slowly, reflecting the doctrine's
 *   capacity to absorb organizational pressure through legal
 *   counter-narrative even as on-the-ground indigenous resistance persists.
 *
 * KEY AGENTS:
 *   - major_holding_institutions: Western museums (British Museum, Louvre, Metropolitan, etc.) set agendas and collect extraction (custody, interpretive authority, resource flows)
 *   - successor_states_with_cultural_claims: Powerful states (Egypt, Greece, Nigeria, India) bear costs of legal action and diplomatic friction; constrained exit
 *   - indigenous_communities_without_institutional_voice: Powerless, trapped, excluded from the legal framework entirely
 *   - international_conservation_expertise: Organized beneficiary producing the standards and knowledge that justify retention
 *   - human_rights_authorities: Observers external to the constraint's enforcement machinery, accumulating pressure on legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, 0.78).
domain_priors:suppression_score(cultural_property_legal_corpus__universal_heritage_reading, 0.71).
domain_priors:theater_ratio(cultural_property_legal_corpus__universal_heritage_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(cultural_property_legal_corpus__universal_heritage_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cultural_property_legal_corpus__universal_heritage_reading, tangled_rope).
narrative_ontology:human_readable(cultural_property_legal_corpus__universal_heritage_reading, "Universal Heritage Reading of Cultural Property Law").
narrative_ontology:topic_domain(cultural_property_legal_corpus__universal_heritage_reading, "international_law/cultural_property/post_colonial_studies").

domain_priors:requires_active_enforcement(cultural_property_legal_corpus__universal_heritage_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(cultural_property_legal_corpus__universal_heritage_reading, 'f995cff9-1481-4aec-8940-9672df090e09').
narrative_ontology:cs_kernel_codification('f995cff9-1481-4aec-8940-9672df090e09', formalized).
narrative_ontology:cs_authority_grounding('f995cff9-1481-4aec-8940-9672df090e09', extraction).
narrative_ontology:cs_interpretation_layer_present('f995cff9-1481-4aec-8940-9672df090e09').
narrative_ontology:cs_reading_relation('f995cff9-1481-4aec-8940-9672df090e09', cultural_property_legal_corpus__sovereign_repatriation_reading, coexists_with).
narrative_ontology:cs_reading_relation('f995cff9-1481-4aec-8940-9672df090e09', cultural_property_legal_corpus__indigenous_stewardship_reading, influences).
narrative_ontology:cs_axiom('f995cff9-1481-4aec-8940-9672df090e09', foundational, cultural_artifacts_are_universal_human_property).
narrative_ontology:cs_axiom_status(cultural_artifacts_are_universal_human_property, holdable).
narrative_ontology:cs_axiom_grounding('f995cff9-1481-4aec-8940-9672df090e09', cultural_artifacts_are_universal_human_property, deontological).
narrative_ontology:cs_axiom('f995cff9-1481-4aec-8940-9672df090e09', foundational, institutional_preservation_superiority_doctrine).
narrative_ontology:cs_axiom_status(institutional_preservation_superiority_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('f995cff9-1481-4aec-8940-9672df090e09', institutional_preservation_superiority_doctrine, empirically_contingent).
narrative_ontology:cs_reference_frame('f995cff9-1481-4aec-8940-9672df090e09', universal_heritage_stewardship_regime).
narrative_ontology:cs_drift_state('f995cff9-1481-4aec-8940-9672df090e09', post_colonial_justice_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f995cff9-1481-4aec-8940-9672df090e09', '').
narrative_ontology:cs_kernel_id(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, international_conservation_expertise).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, successor_states_with_cultural_claims).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities_without_institutional_voice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, source_communities_with_institutional_partnerships).
narrative_ontology:constraint_beneficiary(cultural_property_legal_corpus__universal_heritage_reading, universal_access_constituency).
narrative_ontology:constraint_victim(cultural_property_legal_corpus__universal_heritage_reading, source_communities_with_institutional_partnerships).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Major Western museums, archives, and conservation centers (British Museum, Louvre, Metropolitan Museum, Smithsonian, etc.) hold the largest collections of non-Western cultural artifacts acquired during the colonial era and afterward. They frame themselves as custodians of humanity's heritage and justify retention on grounds of superior preservation capacity and universal scholarly access. They control the institutional infrastructure that defines what constitutes 'proper preservation,' which expertise is legitimate, and what standards stewardship must meet. They actively participate in shaping international cultural property law through expert testimony, advisory roles, and funding of legal arguments. Their primary exit option is arbitrage: they can move collections between institutions, negotiate research partnerships, or reorganize holdings — but abandoning the collection entirely (true repatriation) would constitute exit from their institutional mission. They have strong leverage because the artifacts themselves are held in their custody.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions, beneficiary).

% Nation-states that succeeded former colonies or conquered territories (Egypt, Greece, Nigeria, Mexico, China, India, Turkey, Korea, and dozens of others) claim cultural artifacts held in Western institutions as part of their national patrimony and sovereignty. They seek repatriation through diplomatic channels, legal proceedings, and international advocacy. They bear substantial costs: legal expenses for repatriation claims, diplomatic friction when claims are rejected, and the ongoing symbolic harm of having their national heritage held abroad under foreign custody. Their exit options are constrained to either accepting the permanent foreignness of their heritage or mounting repeated costly disputes with uncertain outcomes. They cannot abandon their cultural claims without domestic political cost.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, successor_states_with_cultural_claims, payer,
    powerful, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, successor_states_with_cultural_claims, excluded).

% Indigenous peoples whose sacred objects, ceremonial items, ancestral remains, and cultural records are held in Western institutions face structural barriers to reclamation: they lack institutional standing in international cultural property law (the universal heritage doctrine treats them as demographic components of successor states, not as autonomous claimants); they cannot fund legal proceedings; and they see their cultural property treated as anthropological specimens and historical artifacts rather than as living sacred material requiring specific ceremonial contexts and stewardship practices. The constraint treats their voice as parochial attachment to particularity rather than as legitimate stewardship claim grounded in cultural continuity. Their exit from the constraint is impossible — they cannot opt out of having their ancestors and sacred objects held abroad.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities_without_institutional_voice, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, indigenous_communities_without_institutional_voice, excluded).

% International conservation science, museum professionals, restoration specialists, and preservation standards-setting bodies (ICOMOS, ICCROM, international conservation associations) benefit from the existence of centralized, professionally-managed collections. They frame repatriation as a threat to preservation capacity and invoke conservation expertise as a primary justification for institutional retention. They produce the technical and scholarly literature, accreditation standards, and training frameworks that define what constitutes 'proper stewardship.' They have organized voice through professional associations and advisory roles in international law. They have exit mobility (conservation expertise is portable and valued across institutions) but their professional identity and career advancement are tied to major-institution employment and involvement in large, centralized collections.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, international_conservation_expertise, beneficiary,
    organized, civilizational, mobile, global).

% Some source communities have negotiated research access agreements, exhibition partnerships, loan arrangements, or co-curatorial roles with holding institutions. They benefit from visibility, scholarly collaboration, and cultural acknowledgment. However, they remain dependent on the institution's continued goodwill and remain structurally barred from permanent custody, unilateral decision-making, or refusing the terms of partnership. Their participation in the universal access framework creates a constituency that defends the institutional model while remaining structurally subordinate to institutional agendas. Their exit options are constrained: they can withdraw from specific partnerships but cannot substantially alter the holding institutions' ownership and control.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, source_communities_with_institutional_partnerships, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(cultural_property_legal_corpus__universal_heritage_reading, source_communities_with_institutional_partnerships, payer).

% Scholars, students, researchers, tourists, and cultural enthusiasts worldwide benefit from centralized, professionally-curated collections available for study without requiring travel to source regions. The universal heritage doctrine frames this constituency as 'humanity' and positions them as the primary beneficiaries of retention. Their actual preference diversity is flattened into a single category. They have exit mobility (they can access information through other means, travel to source regions, or accept reduced access) but benefit from the convenience and curated authority of institutional collections. They are mobilized rhetorically as the constituency served by retention but are not organized as stakeholders in the enforcement of the constraint.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, universal_access_constituency, beneficiary,
    organized, biographical, mobile, global).

% UN bodies, international human rights mechanisms, truth commissions, and post-colonial justice frameworks increasingly scrutinize the universal heritage doctrine as a post-colonial institution that reproduces colonial extraction through law. They take testimony from claimant states and indigenous communities, examine the doctrine's historical roots in European universalism, and can recommend remedies at the international policy level. They are structurally external to the constraint's enforcement machinery but accumulate pressure on its legitimacy through advocacy, precedent-setting, and moral authority. They do not have enforcement power over holding institutions but influence the narrative contestation.
narrative_ontology:constraint_stakeholder(cultural_property_legal_corpus__universal_heritage_reading, human_rights_authorities, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(cultural_property_legal_corpus__universal_heritage_reading, major_holding_institutions).
narrative_ontology:fixing_cost_class(cultural_property_legal_corpus__universal_heritage_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes a single planetary-scale knowledge and preservation infrastructure for non-Western cultural objects: centralizes conservation responsibility in professionally-managed institutions, coordinates multilingual scholarship production, creates universal archival documentation, ensures long-term institutional stewardship against natural decay and conflict-related destruction. Solves the coordination problem of dispersed artifacts where each source region pursuing independent preservation creates fragmentation and duplication of effort; centralization is justified as creating a unified, professionally-maintained record of humanity's cultural diversity.
% TRANSFER_FUNCTION: Moves ownership, physical custody, interpretive authority, and scholarly production capacity from source communities and successor states to Western holding institutions. In exchange for relinquishing these, claimants receive theoretical preservation benefits (institutional stewardship), scholarly access (but mediated through institutional agenda-setting), exhibition opportunities (but curated by institutions), and symbolic inclusion in a 'universal heritage' narrative. The core transfer: loss of autonomous stewardship authority in exchange for promised preservation and scholarly inclusion.
% ABSENT_VOICES: Indigenous communities that would claim sacred stewardship and ceremonial context-requirements are structurally absent from the legal framework — they are represented, if at all, only through successor states rather than as autonomous parties. Alternative preservation models grounded in community practice, sacred protocols, distributed stewardship, and knowledge transmission that requires non-public context are not in the conversation. Post-colonial scholars and indigenous intellectuals arguing that the doctrine is a post-colonial institution rather than a universal principle are heard but systematically over-ruled by the doctrine's enforcement machinery. Voices from successor states advancing repatriation claims are suppressed through legal procedures that defer to possession and international law that privileges institutional stability.
% DISAPPEARANCE_RATIONALE: If the universal heritage doctrine and its enforcement apparatus vanished, holding institutions would face massive repatriation claims; artifacts would disperse to successor states and source communities; institutional preservation capacity would fragment but parallel institutional infrastructure would emerge in regions with sufficient resources and political will; scholarly networks would reorganize around partnership, loan agreements, and digital reproduction rather than permanent Western custody; some artifacts would enter ceremonial circulation within indigenous communities with different preservation and access protocols. The specific form of centralized planetary preservation infrastructure would change; preservation itself would continue in multiple distributed forms with different knowledge frameworks and access protocols.
% FOUNDING_PROBLEM: Colonial-era looting dispersed non-Western cultural artifacts across European and American institutions, fragmenting collections, destroying context, and creating crisis of loss and inaccessibility. Early 20th-century conservation and internationalist movements framed the problem as 'how to preserve humanity's cultural record against decay, war, and dispersal'; the solution developed was centralized, professionally-managed institutional custody under a universal heritage doctrine that made retention the institutional default and repatriation the exception requiring exceptional justification.
% FOUNDING_PROBLEM_CORROBORATION: Western holding institutions and international conservation bodies attest the dispersal crisis is ongoing and universal stewardship through institutional custody is the only adequate solution; they emphasize continuing threats (conflict, environmental degradation, inadequate funding in source regions). Successor states and post-colonial scholars attest the founding problem has been administratively solved and the doctrine now functions as a mechanism for retaining power — preservation capacity has distributed, digital preservation has made geographic centralization less critical for knowledge transmission, and the real remaining 'problem' the doctrine solves is institutional retention of authority and resource flows. International human rights bodies and post-colonial justice mechanisms outside the benefiting institutions corroborate this reframing: the constraint now operates as a mechanism for retaining cultural meaning-making power under the cover of preservation science rather than as a solution to an active preservation crisis.
narrative_ontology:disappearance_verdict(cultural_property_legal_corpus__universal_heritage_reading, world_rearranges).
narrative_ontology:founding_problem_status(cultural_property_legal_corpus__universal_heritage_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(cultural_property_legal_corpus__universal_heritage_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(cultural_property_legal_corpus__universal_heritage_reading, 'none', 1).
narrative_ontology:epsilon_provenance(cultural_property_legal_corpus__universal_heritage_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(cultural_property_legal_corpus__universal_heritage_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(cultural_property_legal_corpus__universal_heritage_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers ownership, custody, and interpretive power from claimants to holding institutions — this is not incidental to preservation, it is the structural core. The beneficiary/victim split is stark: holding institutions and conservation expertise collect preservation authority and scholarly production; claimants bear legal costs, diplomatic friction, and symbolic harm (the harm of having one's heritage treated as a specimen rather than a living patrimony). Suppression (0.71) is high because the doctrine's persistence depends on actively excluding alternative framings — indigenous stewardship claims are suppressed via legal standing rules, sovereign repatriation claims are suppressed via international law that defers to possession, and counter-narratives are suppressed via control of academic discourse and museum authority. Theater (0.42) is moderate and rising: the preservation function is real, but an increasing share of the institutional machinery defends retention against criticism rather than preserving artifacts. The temporal measurements show extraction and suppression rising over 50 years, particularly at the organizational level (state-to-state claims hardening) where the measurement gap widens from 0.58 to 0.71 for suppression and 0.72 to 0.78 for stakes inflation. This reflects growing assertiveness from successor states and indigenous movements, met by hardened institutional defense.
 *
 * PERSPECTIVAL GAP:
 *   From the holding institution seat, this is genuine coordination: a global preservation infrastructure solving a real collective-action problem (dispersed artifacts at risk). From the claimant state seat, this is extraction justified by universalist rhetoric — a post-colonial structure that retains power over cultural meaning under the cover of preservation science. From the indigenous community seat (structurally excluded), this is erasure of living stewardship claims and conversion of sacred objects into anthropological specimens. The engine computes directionality separately for each seat from the structural data: holding institutions get low d (beneficiaries, arbitrage exit); claimants get high d (targeted, constrained exit); communities get highest d (trapped, excluded, identity harm). The claim/metric divergence reflects this conflict: the constraint is CLAIMED as rope by its beneficiaries and designed to appear that way (universal good, genuine preservation coordination); the metrics reflect the extraction and suppression the payers experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Major holding institutions: d near 0.1 (full beneficiary) — they set the rules, collect custody, produce knowledge, face no exit pressure. Successor states: d near 0.82 (near-target) — they bear legal costs, diplomatic friction, constrained exit (accept foreignness of heritage or mount costly disputes). Indigenous communities: d near 0.95 (full target) — trapped, excluded from legal standing, harmed by sacred-object commodification, no exit within the framework. International conservation expertise: d near 0.15 (beneficiary-side) — mobile, professional identity tied to major institutions, benefits from centralized collections. Human rights observers: d near 0.5 (analytical, symmetric) — neither collecting nor paying, external pressure-generating role.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint classifies as tangled_rope under this reading because it possesses both real coordination (planetary preservation infrastructure, conservation science, scholarly access) AND asymmetric extraction (claimants bear costs; institutions collect authority). But the founding problem that justified the constraint (colonial-era dispersal and loss) has substantially shifted: early 20th-century conservation was genuinely responding to crisis; by the early 21st century, preservation capacity has distributed, digital preservation has made geographic centralization less critical, and the founding problem has been administratively solved by now. The remaining extraction (retention of custody, interpretive authority, resource flows) persists because the doctrine has calcified and because holding institutions have institutional interest in retention. This is approaching mandatrophy: the founding problem that justified extraction is no longer live, but extraction persists through doctrinal inertia and active enforcement machinery. The measurement series showing rising extraction and suppression while resistance also rises suggests the constraint is hardening defensively rather than adapting to changed conditions — a sign of mandatrophy progression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    universal_vs_particularist_framing,
    'Is the doctrine''s invocation of ''universal heritage'' and ''shared humanity'' a genuine normative framework for stewardship, or is it a strategic framing that privileges Western institutional interests while delegitimizing non-Western stewardship claims as ''particularist''?',
    'Examine the doctrine''s application: Does it treat all preservation practices (Western institutional, indigenous ceremonial, state-based) as equally valid or does it systematically privilege Western standards? Do successively weaker voices get labeled ''particularist'' while holding-institution preferences get labeled ''universal''?',
    'If the universalism is strategic rather than principled, the constraint is pure extraction wearing a cosmopolitan mask — reclassify from tangled_rope to snare. If genuinely principled but applied asymmetrically, the constraint is tangled_rope with high extraction load due to enforcement of the principle against alternative frameworks.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_vs_particularist_framing, conceptual, 'Whether ''universal heritage'' is a genuine normative framework or a strategic framing privileging Western institutions.').

omega_variable(
    preservation_capacity_empirical_claim,
    'What is the empirical state of preservation capacity distribution? Can successor states and indigenous communities now maintain artifacts adequately, or does the claim of superior Western institutional capacity remain empirically grounded?',
    'Comparative study of preservation outcomes: artifacts held in Western institutions vs. artifacts held in source-region institutions over 30+ years; control for funding differences; assess both material preservation and cultural vitality metrics.',
    'If capacity has genuinely distributed, the extraction is no longer justified by preservation necessity — the constraint has moved into mandatrophy (extraction without founding justification). If Western capacity remains superior, the extraction is extraction of a real good (preservation service), making tangled_rope classification more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preservation_capacity_empirical_claim, empirical, 'Whether preservation capacity distribution now justifies decentralization or whether centralization remains empirically necessary.').

omega_variable(
    indigenous_voice_suppression_mechanism,
    'Is the structural exclusion of indigenous communities from legal standing a consequence of the doctrine or a choice embedded in the doctrine''s application?',
    'Examine international law frameworks: Do they permit indigenous communities to be recognized as parties with standing in cultural property disputes, or do they force indigenous claims through successor-state representation only? Would recognizing indigenous standing change the constraint''s classification or merely its stakeholder base?',
    'If exclusion is structural to the doctrine, indigenous communities are locked into target status. If exclusion is applicational, the constraint could technically admit indigenous voice while preserving the universal-heritage framework — but this would likely increase resistance (measured in the grid as rising suppression requirement).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(indigenous_voice_suppression_mechanism, empirical, 'Whether indigenous voice exclusion is structural to the doctrine or a policy choice.').

omega_variable(
    kernel_reading_foreclosure_question,
    'Does the universal heritage reading logically foreclose the indigenous stewardship reading, or do these represent genuinely coexisting competing frameworks that different parties hold simultaneously?',
    'Test the logical structure: If I hold the universal heritage premise (artifacts serve humanity best as centralized, professionally-managed collections available to all), can I simultaneously hold the indigenous stewardship premise (artifacts belong to communities maintaining cultural continuity)? Can both be true in a single framework, or does accepting one require rejecting the other?',
    'If they foreclose each other, the readings are structurally incompatible and the kernel_reading relation is ''forecloses''. If they coexist as different parties'' competing commitments without logical contradiction within each party''s framework, the relation is ''coexists_with''. If this reading creates structural conditions that pressure the indigenous reading (e.g., by controlling academic legitimacy), the relation is ''influences''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_question, conceptual, 'Whether the universal heritage and indigenous stewardship readings logically foreclose each other or coexist as live alternatives.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cultural_property_legal_corpus__universal_heritage_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cult_tr_t0, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(cult_tr_t8, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(cult_tr_t16, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(cult_tr_t25, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(cult_tr_t35, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(cult_tr_t50, cultural_property_legal_corpus__universal_heritage_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(cult_be_t0, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(cult_be_t8, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(cult_be_t16, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 16, 0.71).
narrative_ontology:measurement(cult_be_t25, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 25, 0.75).
narrative_ontology:measurement(cult_be_t35, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 35, 0.77).
narrative_ontology:measurement(cult_be_t50, cultural_property_legal_corpus__universal_heritage_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(cult_su_t0, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(cult_su_t8, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(cult_su_t16, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(cult_su_t25, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(cult_su_t35, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(cult_su_t50, cultural_property_legal_corpus__universal_heritage_reading, suppression_requirement, 50, 0.71).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(cult_grid_01, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(class), 0, 0.7).
narrative_ontology:measurement(cult_grid_02, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(class), 50, 0.65).
narrative_ontology:measurement(cult_grid_03, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(cult_grid_04, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(individual), 50, 0.48).
narrative_ontology:measurement(cult_grid_05, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(cult_grid_06, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(organizational), 50, 0.62).
narrative_ontology:measurement(cult_grid_07, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(cult_grid_08, cultural_property_legal_corpus__universal_heritage_reading, accessibility_collapse(structural), 50, 0.68).
narrative_ontology:measurement(cult_grid_09, cultural_property_legal_corpus__universal_heritage_reading, resistance(class), 0, 0.71).
narrative_ontology:measurement(cult_grid_10, cultural_property_legal_corpus__universal_heritage_reading, resistance(class), 50, 0.72).
narrative_ontology:measurement(cult_grid_11, cultural_property_legal_corpus__universal_heritage_reading, resistance(individual), 0, 0.65).
narrative_ontology:measurement(cult_grid_12, cultural_property_legal_corpus__universal_heritage_reading, resistance(individual), 50, 0.68).
narrative_ontology:measurement(cult_grid_13, cultural_property_legal_corpus__universal_heritage_reading, resistance(organizational), 0, 0.82).
narrative_ontology:measurement(cult_grid_14, cultural_property_legal_corpus__universal_heritage_reading, resistance(organizational), 50, 0.78).
narrative_ontology:measurement(cult_grid_15, cultural_property_legal_corpus__universal_heritage_reading, resistance(structural), 0, 0.58).
narrative_ontology:measurement(cult_grid_16, cultural_property_legal_corpus__universal_heritage_reading, resistance(structural), 50, 0.68).
narrative_ontology:measurement(cult_grid_17, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(cult_grid_18, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(class), 50, 0.75).
narrative_ontology:measurement(cult_grid_19, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(individual), 0, 0.45).
narrative_ontology:measurement(cult_grid_20, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(individual), 50, 0.52).
narrative_ontology:measurement(cult_grid_21, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(cult_grid_22, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(organizational), 50, 0.78).
narrative_ontology:measurement(cult_grid_23, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(structural), 0, 0.58).
narrative_ontology:measurement(cult_grid_24, cultural_property_legal_corpus__universal_heritage_reading, stakes_inflation(structural), 50, 0.65).
narrative_ontology:measurement(cult_grid_25, cultural_property_legal_corpus__universal_heritage_reading, suppression(class), 0, 0.75).
narrative_ontology:measurement(cult_grid_26, cultural_property_legal_corpus__universal_heritage_reading, suppression(class), 50, 0.78).
narrative_ontology:measurement(cult_grid_27, cultural_property_legal_corpus__universal_heritage_reading, suppression(individual), 0, 0.48).
narrative_ontology:measurement(cult_grid_28, cultural_property_legal_corpus__universal_heritage_reading, suppression(individual), 50, 0.52).
narrative_ontology:measurement(cult_grid_29, cultural_property_legal_corpus__universal_heritage_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(cult_grid_30, cultural_property_legal_corpus__universal_heritage_reading, suppression(organizational), 50, 0.75).
narrative_ontology:measurement(cult_grid_31, cultural_property_legal_corpus__universal_heritage_reading, suppression(structural), 0, 0.62).
narrative_ontology:measurement(cult_grid_32, cultural_property_legal_corpus__universal_heritage_reading, suppression(structural), 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cultural_property_legal_corpus__universal_heritage_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(cultural_property_legal_corpus__universal_heritage_reading, 0.22).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__sovereign_repatriation_reading).
narrative_ontology:affects_constraint(cultural_property_legal_corpus__universal_heritage_reading, cultural_property_legal_corpus__indigenous_stewardship_reading).

% DUAL FORMULATION NOTE:
% This story (universal_heritage_reading) is one reading of the cultural_property_legal_corpus kernel. It must be understood in relation to the sibling readings: sovereign_repatriation_reading (successor states hold authority) and indigenous_stewardship_reading (communities maintain cultural continuity). Each reading produces a different constraint with different ε, different beneficiaries/victims, different classifications. The three stories are not variations on a single constraint — they are three structurally distinct constraints instantiated by three incompatible frameworks applied to the same domain. Links to siblings via network.affects_constraints document the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cultural_property_legal_corpus__universal_heritage_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
