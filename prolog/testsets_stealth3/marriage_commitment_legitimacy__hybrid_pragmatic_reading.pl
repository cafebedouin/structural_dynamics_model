% ============================================================================
% CONSTRAINT STORY: marriage_commitment_legitimacy__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_legitimacy__hybrid_pragmatic_reading, []).

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
 *   constraint_id: marriage_commitment_legitimacy__hybrid_pragmatic_reading
 *   human_readable: Manifesto Compliance Regime (1890-1924) — Hybrid Pragmatic Reading: Strategic Institutional Adaptation
 *   domain: religious_institutional_history/political_theology/commitment_systems
 *
 * SUMMARY:
 *   The 1890 Woodruff Manifesto and the enforcement regime built around it
 *   suspend the practice of plural marriage while leaving the doctrine that
 *   commands it canonized. Under the hybrid pragmatic reading — the reading
 *   instantiated here — the Manifesto is strategic institutional adaptation:
 *   prophetic authority deployed to end a federal existential threat while
 *   preserving core theological commitments through deliberate scope
 *   ambiguity. The ε referent is the standing arrangement under contest — the
 *   Manifesto compliance regime as it actually operated, including the covert
 *   post-1890 authorizations and the post-1904 enforcement turn — assessed by
 *   this reading's own lights, never by the arrangement this reading would
 *   have preferred. Assumptions stated: the interval is anchored to calendar
 *   years; all metric values are historiographic estimates from the
 *   documentary record (diaries, meeting minutes, Senate testimony, sealing
 *   ledgers), not instrument readings. This story is one of three readings of
 *   the kernel marriage_commitment_legitimacy; the siblings
 *   (exogenous_override, endogenous_reinterpretation) are separate
 *   constraints with their own ε and victim structures, linked via
 *   network.affects_constraints per the ε-invariance decomposition rule. The
 *   claimed type and the metrics are authored independently: the claim is
 *   tangled_rope because the arrangement demonstrably solved a collective
 *   legal-survival problem while transferring legitimacy and certainty away
 *   from the members whose marriages were its object; the metrics describe
 *   the observed operation without being tuned to that claim.
 *
 * KEY AGENTS:
 *   - first_presidency_leadership: agenda-setting beneficiary seat (institutional/arbitrage) — issues and administers the regime, retains doctrinal ownership and exception discretion, collects the preserved flexibility and institutional survival
 *   - quorum_of_twelve_leadership: secondary beneficiary seat with a minority payer component (institutional/constrained) — shares the institutional gains; two apostles bear discipline for post-1890 sealings
 *   - general_membership: diffuse beneficiary with payer residue (organized/constrained) — receives survival, statehood, and peace; funds the defense and carries the interpretive burden
 *   - rank_and_file_plural_families: primary target (moderate/identity_locked) — bears the redefinition of marriages performed under prior prophetic command
 *   - covert_practitioners: hidden target (powerless/trapped) — bear prosecution and excommunication for relying on private authorizations the same authority publicly disavowed
 *   - mexican_colony_families: peripheral target (powerless/trapped) — bear dissolution orders delivered to colonies built as a legal refuge
 *   - dissenting_apostles: elite target (powerful/identity_locked) — bear the enforcement turn; their discipline teaches the cost of open objection
 *   - federal_authorities: external beneficiary (institutional/mobile) — receives compliance and the statehood settlement; holds the coercive backdrop
 *   - church_historians: analytical observer (analytical/analytical) — sees the full structure of public text, private exceptions, and enforcement turn
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58).
domain_priors:suppression_score(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(marriage_commitment_legitimacy__hybrid_pragmatic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "Manifesto Compliance Regime (1890-1924) — Hybrid Pragmatic Reading: Strategic Institutional Adaptation").
narrative_ontology:topic_domain(marriage_commitment_legitimacy__hybrid_pragmatic_reading, "religious_institutional_history/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(marriage_commitment_legitimacy__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'd97bcdf1-022a-4ec8-b291-fa51f4dcdb90').
narrative_ontology:cs_kernel_codification('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', fixed_text).
narrative_ontology:cs_authority_grounding('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', lineage).
narrative_ontology:cs_interpretation_layer_present('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90').
narrative_ontology:cs_reading_relation('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', marriage_commitment_legitimacy__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', marriage_commitment_legitimacy__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', foundational, prophetic_authority_strategically_deployed).
narrative_ontology:cs_axiom_status(prophetic_authority_strategically_deployed, holdable).
narrative_ontology:cs_axiom_grounding('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', prophetic_authority_strategically_deployed, instrumental).
narrative_ontology:cs_axiom('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', foundational, doctrine_preserved_practice_suspended).
narrative_ontology:cs_axiom_status(doctrine_preserved_practice_suspended, holdable).
narrative_ontology:cs_axiom_grounding('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', doctrine_preserved_practice_suspended, theological).
narrative_ontology:cs_reference_frame('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', adaptive_prophetic_governance).
narrative_ontology:cs_drift_state('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', contemporary_post_schism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d97bcdf1-022a-4ec8-b291-fa51f4dcdb90', '').
narrative_ontology:cs_kernel_id(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, general_membership).
narrative_ontology:constraint_beneficiary(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authorities).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_plural_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, covert_practitioners).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, mexican_colony_families).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dissenting_apostles).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_leadership).
narrative_ontology:constraint_victim(marriage_commitment_legitimacy__hybrid_pragmatic_reading, general_membership).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prophetic_adaptability_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_legitimacy__hybrid_pragmatic_reading, institutional_preservation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the 1890 declaration and administers its application. Retains ownership of the plural-marriage doctrine — the 1843 revelation stays canonized — while directing members to comply with federal law. For roughly a decade continues to authorize a limited number of new plural marriages for trusted applicants while publicly professing compliance, then closes that channel under Senate pressure in 1904. Exit is effectively open: the presidency controls the text, its interpretation, and the pace of enforcement, so it can reframe the declaration's meaning as pressure shifts. It collects the arrangement's gains: institutional survival, preserved doctrinal flexibility, and discretion over exceptions.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership, beneficiary).

% Shares in the preserved institution and its authority; the declaration's success keeps the quorum intact, its members' property interests secure, and the missionary field open. A minority — two apostles who performed or defended post-1890 sealings — bear the costs when enforcement hardens: pressured resignations, loss of calling, eventual excommunication. Exit is narrowed by office: leaving the quorum means losing the calling that constitutes their standing.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_leadership, beneficiary,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, quorum_of_twelve_leadership, payer).

% The majority of members, not living in plural marriage, receive the arrangement's main benefits: an intact church, restored property, Utah statehood, and an end to raids and prosecutions. They also fund the legal defense through tithing, absorb the interpretive burden of a doctrine that remains canon but is not practiced, and accept discipline from a leadership whose decisions they cannot review. Exit means leaving the covenant community that constitutes their religious and social world.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, general_membership, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_legitimacy__hybrid_pragmatic_reading, general_membership, payer).

% Families already living in plural marriage when the declaration issued. Told that marriages performed under prior prophetic command remain valid but that no new ones may be contracted; some husbands face prosecution for continuing cohabitation; wives and children carry the legal and social exposure. Leaving the faith means abandoning the community, the theology, and the sealing bonds that constitute the family's eternal standing, so most stay and comply while the meaning of their own marriages is redefined over their heads.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, rank_and_file_plural_families, payer,
    moderate, biographical, identity_locked, continental).

% Members who enter or continue plural marriages after 1890, some with private leadership authorization. When enforcement hardens after 1904 they face church courts, excommunication, and in some cases federal prosecution. Their position is the arrangement's hidden cost surface: they relied on the same authority that publicly disavowed them, and by the time the channel closed they had families that could not be undone.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, covert_practitioners, payer,
    powerless, biographical, trapped, continental).

% Families who moved to Mormon colonies in northern Mexico, where plural marriage continued under local authorization after 1890. When the 1904 tightening reaches them, they are instructed to dissolve recent unions; some comply at severe family cost, others refuse and are cut off. Exit options are thin: returning to the United States means prosecution exposure, remaining means schism, and the Mexican revolution soon destroys the colonies' refuge value anyway.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, mexican_colony_families, payer,
    powerless, biographical, trapped, regional).

% Senior apostles who performed or defended post-Manifesto sealings and resisted the 1904 tightening. They testify before the Senate committee, are pressured into resigning from the quorum, and are ultimately excommunicated or disfellowshipped. Their identity is fused with the office they hold, so exit means self-erasure; their resistance is absorbed rather than accommodated, and their fate teaches the remaining membership the cost of open objection.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dissenting_apostles, payer,
    powerful, biographical, identity_locked, continental).

% Congress, the Justice Department, and the Senate committee that pursued the church over plural marriage. They receive what they demanded: cessation of the practice, resolution of the church's property claims, and a territorial population that can be admitted to statehood under monogamous law. They remain outside the arrangement's governance but hold the coercive backdrop — confiscation, imprisonment, disfranchisement — that makes compliance rational, and they can resume it at will.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, federal_authorities, beneficiary,
    institutional, generational, mobile, national).

% Scholars inside and outside the tradition who reconstruct the decision record from diaries, First Presidency meeting minutes, sealing ledgers, and Senate testimony. They see the full structure — the public text, the private exceptions, the enforcement turn — and publish readings the institution neither adopts nor refutes.
narrative_ontology:constraint_stakeholder(marriage_commitment_legitimacy__hybrid_pragmatic_reading, church_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_legitimacy__hybrid_pragmatic_reading, first_presidency_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_legitimacy__hybrid_pragmatic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a collective legal-survival problem no member could solve individually: aligning the whole community's marriage practice with federal law in one authoritative act, ending prosecutions and property seizure, securing amnesty and Utah statehood, and keeping the church's corporate assets, missionary network, and leadership structure intact.
% TRANSFER_FUNCTION: Moved marital legitimacy and interpretive certainty from rank-and-file plural families to institutional leadership: families surrendered the practice, and later the authorized exceptions, while the presidency retained ownership of the doctrine, discretion over its future, and the institutional assets the compliance preserved; the federal government received compliance and the political settlement it demanded.
% ABSENT_VOICES: Women in plural marriages and children of post-1890 unions had no seat when the declaration was framed or when the 1904 tightening applied to them; their exposure was decided by others. Dissenting apostles spoke but were progressively marginalized between 1904 and 1911. Mexican and Canadian colony members learned of the tightening's application after it was decided. The federal state was present only as pressure — it dictated terms and never negotiated them.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight — say, in 1904 — federal authorities would have resumed confiscation and prosecution against an unrepentant institution, the quorum would have faced imprisonment, and the colonies would have lost their legal refuge. Members' marriage arrangements, the fundamentalist schism that later absorbed dissent, Utah's statehood settlement, and the church's territorial political position all presuppose the arrangement's operation; nothing about the world of 1904 stays put if it disappears.
% FOUNDING_PROBLEM: Federal criminalization of plural marriage (Edmunds Act 1882, Edmunds-Tucker Act 1887) put the church's corporate property, its leaders' liberty, and its members' voting rights under existential threat; the arrangement was built to end the practice without surrendering the doctrine that commanded it.
% FOUNDING_PROBLEM_CORROBORATION: The legal crisis is attested dead by sources outside the beneficiary set: the amnesty proclamations, the 1896 statehood act, and the lapse of federal prosecutions after compliance. No party outside the benefiting seats attests that the founding legal problem remained live at interval end. The strongest corroboration is the arrangement's own behavior: enforcement intensified after 1904, after the founding problem was solved — a signature of an arrangement outliving its founding problem. The continuing canonical tension around the 1843 revelation is a successor problem the arrangement left unresolved, not the founding one.
narrative_ontology:disappearance_verdict(marriage_commitment_legitimacy__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_legitimacy__hybrid_pragmatic_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_legitimacy__hybrid_pragmatic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_legitimacy__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) is moderate: the arrangement's costs concentrate on plural families, covert practitioners, and colonists, while the majority of members net-benefit from survival, statehood, and an end to prosecution — the tangled-rope signature of genuine coordination carrying asymmetric extraction. Suppression (0.55) is real but late: the 1890 declaration was initially accepted with little internal coercion, and the enforcement machinery — church courts, pressured apostolic resignations, excommunications — was built mainly after 1904, which is why suppression_requirement rises from 0.35 to 0.70 before settling at 0.55 as dissent exits into schism and enforcement normalizes. Theater (0.38) peaks at 0.52 around the Reed Smoot hearings (1904-1908), when public compliance performance and private authorization ran furthest apart, then declines as practice converges on the public doctrine. Accessibility collapse is moderate (0.52): alternatives existed — emigration to Mexico and Canada, covert continuation, eventual fundamentalist exit — but each was progressively closed between 1904 and 1912. Resistance (0.58) is substantial and came from inside the elite as well as the rank and file, but the target seats were fragmented across geography and family situation; no coalition formed before the fundamentalist schism, and the schism was exit rather than coalition. All three series share one time grid (1890, 1896, 1900, 1904, 1908, 1911, 1918, 1924); no suppression series is authored beyond the shared grid because the enforcement picture is dynamic, not static.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute differently. From the First Presidency's position the arrangement is stewardship: a choice between institutional death and a disciplined retreat that preserved the church, its properties, and its missionary future, with ambiguity as the price of holding a canonized doctrine and a federal legal order in one frame. From the plural-family seats the same arrangement is a broken covenant: marriages commanded by the same authority that now disavows them, with the interpretive burden — revelation or politics? — left unanswerable. The same-level divergence between general_membership and rank_and_file_plural_families (both organized members of the same body at comparable standing) is driven entirely by position relative to the suspended practice: the costs fall on whichever families were living it, so nominal equals sit on opposite sides of the transfer. The quorum's internal split (most share the gains; two bear discipline) is the elite-level version of the same divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The presidency sits near the beneficiary end of d: it collects institutional survival, retained doctrinal ownership, and exception discretion, and its exit is arbitrage-grade because it controls the text and its interpretation. The quorum derives mostly beneficiary directionality with a payer component from the disciplined minority; general membership sits near symmetric — genuine coordination benefit against tithing-funded costs and interpretive burden. Federal authorities receive a genuine but external benefit (compliance, statehood path) with mobile exit. The target seats — plural families, covert practitioners, Mexican colonists, dissenting apostles — carry high d: their marital legitimacy or standing was the object transferred, and their exits range from identity_locked (sealing theology, apostolic office) to trapped (prosecution exposure, colony isolation), placing them near the full-target end. No directionality overrides are authored: the beneficiary/victim declarations plus exit options already produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — federal criminalization threatening confiscation, imprisonment, and disfranchisement — was dead by 1896, yet enforcement intensified after 1904 and the arrangement's machinery outlived its founding problem. The founding_problem_status (dead) × disappearance_verdict (world_rearranges) mismatch is therefore expected under this reading and should fire the capture/zombie cross-check: the hybrid reading predicts exactly this trajectory, where strategic adaptation succeeds, the coordination function completes, and the enforcement apparatus turns inward on dissent. The tangled_rope classification is what prevents the two mislabelings the sibling readings invite: the endogenous reading would render the arrangement pure coordination and erase the extraction borne by plural families; the exogenous reading would render it pure duress and erase both the genuine collective-survival function and the discretion the leadership preserved for itself. The residual post-1924 enforcement against schismatic groups is a successor arrangement, not this one, and should be authored separately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading (hybrid_pragmatic) of the kernel marriage_commitment_legitimacy; what would the sibling readings change structurally if adopted as the authoring seat?',
    'Re-author the story from each sibling''s seat and compare: the endogenous reading would lower ε toward coordination cost (genuine revelation makes member sacrifice coherent) and remove the leadership beneficiary; the exogenous reading would keep the victims but relocate the transfer to duress, dropping scope-ambiguity discretion from the beneficiary side.',
    'The kernel''s constraint family classifies differently per reading: hybrid computes tangled_rope; endogenous computes rope-ward; exogenous computes snare-ward. Cross-reading comparison is the measurement; no single story adjudicates the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel contest: three readings of the Manifesto with divergent epsilon and victim structures.').

omega_variable(
    deliberate_ambiguity_vs_emergent_confusion,
    'Was the Manifesto''s scope ambiguity a deliberate strategic instrument (this reading''s core claim), or did it emerge from genuine leadership uncertainty about the doctrine''s status?',
    'Archival record: Woodruff''s diaries, First Presidency meeting minutes, and the authorization ledgers for post-1890 sealings. Deliberate strategy shows as coordinated public messaging alongside disciplined private exceptions; confusion shows as inconsistent rulings and retrospective rationalization.',
    'If emergent, the arrangement moves rope-ward (coordination without strategic extraction) and the theater peak is re-read as institutional drift rather than managed performance; if deliberate, the tangled_rope reading holds and the extraction was intentional rent on ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_ambiguity_vs_emergent_confusion, empirical, 'Whether the scope ambiguity was strategic design or emergent confusion — the crux of this reading''s claim.').

omega_variable(
    post_manifesto_authorization_extent,
    'How many plural marriages were performed with leadership authorization after the 1890 declaration while compliance was publicly professed?',
    'Scholarly reconstruction from sealing records, mission and colony records, and contemporaneous testimony (Senate hearings, church court minutes); estimates in the historical literature vary widely.',
    'A larger authorized volume raises the theater trajectory''s peak and the beneficiary seat''s effective extraction; a small volume lowers both and moves the story toward the exogenous sibling''s picture of the event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_manifesto_authorization_extent, empirical, 'Extent of covert post-Manifesto authorizations — the size of the arrangement''s performance gap.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was member compliance with the enforcement turn structural (prosecution exposure, excommunication risk, community loss) or internalized (belief that prophetic declaration binds regardless of its source)?',
    'Post-exit suppression trajectory: fundamentalist communities that left carried the authority framework with them — their own prophets, their own plural practice. Persistence of the authority structure after exit indicates a substantial internalized component.',
    'If largely internalized, effective suppression exceeds the structural measure and the target seats'' exit options are worse than authored; if largely structural, suppression tracks enforcement capacity and falls with it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in member compliance with the enforcement turn.').

omega_variable(
    beneficiary_structure_contest,
    'Did the leadership seat benefit strategically from the arrangement, or did the whole community benefit from survival with leadership merely holding unavoidable discretion?',
    'Distributional and counterfactual analysis: who bore the costs of the ambiguity (plural families, dissenters) versus who held the options it preserved (the presidency''s control of text and timing); outside corroboration from the Senate record and contemporaneous dissenting testimony.',
    'If benefit was diffuse, the leadership seat''s effective extraction drops toward coordination overhead and the story moves rope-ward; if concentrated, the tangled_rope reading holds with the presidency as the receipt seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_contest, conceptual, 'Whether the beneficiary structure is concentrated in leadership or diffuse across the community.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_legitimacy__hybrid_pragmatic_reading, 1890, 1924).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcleg_hybrid_tr_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1890, 0.25).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1890, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1896, 0.3).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1896, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1900, 0.38).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1900, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1904, 0.5).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1904, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1908, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1908, 0.52).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1908, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1911, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1911, 0.48).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1911, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1918, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1918, 0.42).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1918, observed).
narrative_ontology:measurement(mcleg_hybrid_tr_t1924, marriage_commitment_legitimacy__hybrid_pragmatic_reading, theater_ratio, 1924, 0.38).
narrative_ontology:measurement_basis(mcleg_hybrid_tr_t1924, observed).

% Extraction over time
narrative_ontology:measurement(mcleg_hybrid_be_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1890, 0.42).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1890, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1896, 0.46).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1896, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1900, 0.52).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1900, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1904, 0.6).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1904, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1908, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1908, 0.63).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1908, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1911, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1911, 0.62).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1911, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1918, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1918, 0.59).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1918, observed).
narrative_ontology:measurement(mcleg_hybrid_be_t1924, marriage_commitment_legitimacy__hybrid_pragmatic_reading, base_extractiveness, 1924, 0.58).
narrative_ontology:measurement_basis(mcleg_hybrid_be_t1924, observed).

% Suppression requirement over time
narrative_ontology:measurement(mcleg_hybrid_su_t1890, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1890, 0.35).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1890, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1896, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1896, 0.38).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1896, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1900, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1900, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1904, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1904, 0.58).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1904, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1908, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1908, 0.65).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1908, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1911, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1911, 0.7).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1911, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1918, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1918, 0.62).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1918, observed).
narrative_ontology:measurement(mcleg_hybrid_su_t1924, marriage_commitment_legitimacy__hybrid_pragmatic_reading, suppression_requirement, 1924, 0.55).
narrative_ontology:measurement_basis(mcleg_hybrid_su_t1924, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_legitimacy__hybrid_pragmatic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_legitimacy__hybrid_pragmatic_reading, marriage_commitment_legitimacy__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the ε-invariance principle: the natural-language label 'the Manifesto' covers three structurally distinct claims about one act. This story instantiates the hybrid pragmatic reading (strategic adaptation through scope ambiguity, ε ≈ 0.58, leadership beneficiary, plural-family victims). The exogenous override sibling (pure federal coercion over unchanged doctrine) has a different ε and no strategic beneficiary; the endogenous reinterpretation sibling (genuine revelation) has near-coordination ε and no victim set in the same sense. Each reading gets its own stable ε, beneficiaries, and victims; the family links here carry the decomposition. The upstream sibling (endogenous) is the institution's official frame and supplies the legitimacy conditions this reading operates against.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
