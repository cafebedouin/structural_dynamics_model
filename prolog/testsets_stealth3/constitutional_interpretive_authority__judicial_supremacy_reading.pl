% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy: Final Interpretive Authority and Legislative Nullification
 *   domain: constitutional law/political theory/jurisprudence
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   constitutional_interpretive_authority: the judicial_supremacy_reading,
 *   under which courts possess final interpretive authority as guardians of
 *   the fundamental law and legislative acts are subject to judicial
 *   nullification. The epsilon referent is the standing judicial-supremacy
 *   arrangement itself, assessed by this reading's own lights — not the
 *   parliamentary or coordinate arrangements the sibling readings would
 *   install. Structurally the arrangement coordinates (terminating
 *   constitutional disputes, enforcing entrenched rights) while extracting
 *   asymmetrically (interpretive authority accrues to the court; the
 *   legislature's enacted work is revocable at judicial discretion;
 *   democratic outcomes are overridable in the name of rights-compliance
 *   rather than democratic will). The sibling readings —
 *   parliamentary_supremacy_reading and coordinate_construction_reading — are
 *   separate constraints with their own epsilon values and victim sets,
 *   linked through network.affects_constraints; nothing about them is
 *   averaged into this file. KEY AGENTS (by structural relationship):
 *   apex_constitutional_court — agenda-setter and primary beneficiary
 *   (institutional/identity_locked), administers nullification and collects
 *   interpretive authority; national_legislature — primary target
 *   (institutional/constrained), its enactments voidable after passage;
 *   democratic_electorate — split seat (organized/trapped), gains rights
 *   protection, loses policy control; minority_rights_communities — protected
 *   beneficiary (powerless/trapped), relies on nullification as shield;
 *   subnational_legislatures — secondary target (organized/constrained),
 *   regional law reviewed and frequently voided; legal_professoriate_and_bar
 *   — incidental beneficiary (organized/mobile), careers built on expounding
 *   binding doctrine; comparative_constitutional_scholars — analytical
 *   observer (analytical/analytical).
 *
 * KEY AGENTS:
 *   - apex_constitutional_court: agenda-setter and primary beneficiary (institutional/identity_locked) — decides which statutes stand, collects interpretive authority as the arrangement's direct yield
 *   - national_legislature: primary target (institutional/constrained) — enacts policy subject to ex post judicial annulment; counter-moves all slow or themselves reviewable
 *   - democratic_electorate: split seat (organized/trapped) — wins rights protection it could not legislate, loses ballot-box outcomes judges strike
 *   - minority_rights_communities: protected beneficiary (powerless/trapped) — nullification is their working shield against majoritarian legislation
 *   - subnational_legislatures: secondary target (organized/constrained) — regional law voided under the center's final interpretive word
 *   - legal_professoriate_and_bar: incidental beneficiary (organized/mobile) — prestige and expertise markets fed by the interpretive monopoly
 *   - comparative_constitutional_scholars: analytical observer (analytical/analytical) — documents how rival settlements perform across polities
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, 0.56).
domain_priors:suppression_score(constitutional_interpretive_authority__judicial_supremacy_reading, 0.6).
domain_priors:theater_ratio(constitutional_interpretive_authority__judicial_supremacy_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__judicial_supremacy_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy: Final Interpretive Authority and Legislative Nullification").
narrative_ontology:topic_domain(constitutional_interpretive_authority__judicial_supremacy_reading, "constitutional law/political theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__judicial_supremacy_reading, 'ce9ba38f-4034-4e93-895f-cf89a839dfa9').
narrative_ontology:cs_kernel_codification('ce9ba38f-4034-4e93-895f-cf89a839dfa9', fixed_text).
narrative_ontology:cs_authority_grounding('ce9ba38f-4034-4e93-895f-cf89a839dfa9', lineage).
narrative_ontology:cs_interpretation_layer_present('ce9ba38f-4034-4e93-895f-cf89a839dfa9').
narrative_ontology:cs_reading_relation('ce9ba38f-4034-4e93-895f-cf89a839dfa9', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce9ba38f-4034-4e93-895f-cf89a839dfa9', constitutional_interpretive_authority__coordinate_construction_reading, influences).
narrative_ontology:cs_axiom('ce9ba38f-4034-4e93-895f-cf89a839dfa9', foundational, paramount_law_requires_final_human_expositor).
narrative_ontology:cs_axiom_status(paramount_law_requires_final_human_expositor, holdable).
narrative_ontology:cs_axiom_grounding('ce9ba38f-4034-4e93-895f-cf89a839dfa9', paramount_law_requires_final_human_expositor, deontological).
narrative_ontology:cs_axiom('ce9ba38f-4034-4e93-895f-cf89a839dfa9', foundational, electoral_insulation_preserves_constitutional_fidelity).
narrative_ontology:cs_axiom_status(electoral_insulation_preserves_constitutional_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('ce9ba38f-4034-4e93-895f-cf89a839dfa9', electoral_insulation_preserves_constitutional_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('ce9ba38f-4034-4e93-895f-cf89a839dfa9', paramount_written_law_with_final_judicial_expositor).
narrative_ontology:cs_drift_state('ce9ba38f-4034-4e93-895f-cf89a839dfa9', contemporary_rule_of_law_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ce9ba38f-4034-4e93-895f-cf89a839dfa9', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, minority_rights_communities).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, legal_professoriate_and_bar).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, national_legislature).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_electorate).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__judicial_supremacy_reading, subnational_legislatures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_electorate).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, judicial_review_legitimacy_doctrine).
narrative_ontology:constraint_vindicates(constitutional_interpretive_authority__judicial_supremacy_reading, counter_majoritarian_guardianship_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which statutes stand and which fall; writes the binding account of what the constitution requires; its precedents bind every other institution. Every nullification adds to the stock of questions only it may answer, so interpretive authority accumulates to it as the direct yield of the arrangement. Its members hold long or life tenure, answer to no electorate, and can be checked only by amendment, appointment turnover, or open defiance — all slow, all costly. Abandoning the guardian role is not a live option: the court's authority and its members' professional self-understanding are constituted by it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court, beneficiary).

% Drafts and enacts statutory policy knowing any act can be annulled on constitutional grounds after passage, sometimes decades later. Its own interpretive claims about the constitution carry no independent weight once the court has spoken. Counter-moves exist — constitutional amendment, jurisdiction-stripping, appointment politics, deliberate defiance — but each is supermajoritarian, slow, or itself subject to judicial gloss. It cannot exit the constitution; it can only fight inside it.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, national_legislature, payer,
    institutional, biographical, constrained, national).

% Elects the legislature whose products the court may void. It gains rights protection it could not reliably secure legislatively, and loses policy contests it won at the ballot box whenever judges strike the winning statute. Apart from emigration there is no exit from the jurisdiction's interpretive settlement; recourse runs through appointment politics and amendment, both mediated by elites.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_electorate, payer,
    organized, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(constitutional_interpretive_authority__judicial_supremacy_reading, democratic_electorate, beneficiary).

% Relies on judicial nullification as the working shield against majoritarian legislation targeting them, and historically are the clearest net winners from the arrangement. Their protection depends on doctrinal majorities they cannot elect or remove, so the same insulation that shields them can withdraw the shield without appeal.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, minority_rights_communities, beneficiary,
    powerless, generational, trapped, national).

% In federal arrangements, enacts regional law that the apex court reviews and frequently voids for conflict with the national constitution as the court reads it. Regional interpretive disagreement carries no weight against the center's final word. Exit would mean secession-level politics; counter-moves run through the same national amendment machinery that favors incumbents of the settlement.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, subnational_legislatures, payer,
    organized, biographical, constrained, regional).

% Careers, prestige, and the market for legal expertise are built on expounding the court's doctrine; the interpretive monopoly generates the very material — cases, doctrines, clerkships — the profession consumes. Members are free to criticize particular decisions while remaining structurally invested in the finality that makes doctrine authoritative.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, legal_professoriate_and_bar, beneficiary,
    organized, biographical, mobile, national).

% Track how different democracies settle the same question — some vesting finality in courts, some in parliaments, some in ongoing inter-branch dialogue — and document the trade-offs each settlement produces. They hold no stake in any single polity's arrangement beyond the analysis itself.
narrative_ontology:constraint_stakeholder(constitutional_interpretive_authority__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_interpretive_authority__judicial_supremacy_reading, apex_constitutional_court).
narrative_ontology:fixing_cost_class(constitutional_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single terminating authority for disputes about the fundamental law, so constitutional conflicts conclude instead of cycling indefinitely between branches, and enforces entrenched rights constraints against transient legislative majorities.
% TRANSFER_FUNCTION: Moves interpretive finality — and with it an ex post veto over enacted policy — from the elected legislature to the unelected apex court. Each nullification converts a legislative majority's outcome into a judicial majority's ruling, and precedent accumulates as a growing stock of questions only the court may answer.
% ABSENT_VOICES: Voters and legislators whose enactments are struck appear only as losing litigants; citizens favoring parliamentary or dialogic settlements have no seat inside a court-defined forum, and the forum's gatekeeping is itself part of the arrangement under assessment. Future cohorts bound by today's doctrine are present only as abstractions.
% DISAPPEARANCE_RATIONALE: If nullification authority vanished overnight, final interpretive authority would revert to the legislature or fragment across branches; the accumulated case-law edifice constraining every other institution would lose its enforcer; minority protections currently delivered by statute-striking would need rebuilding through political channels; and the legal profession's authority structure would reorganize around whatever settlement replaced it.
% FOUNDING_PROBLEM: Under pure legislative supremacy, the constitution's constraints are enforceable only by the legislature's self-restraint: transient majorities can violate minority rights, and disputes over the meaning of the fundamental law have no terminating authority, cycling between branches until political force settles them.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by comparative political science, which documents both persistent majoritarian rights violations in non-review polities and the fact that some parliamentary systems terminate the same disputes without judicial nullification; by the historical record of pre-judicial-review constitutions; and by counter-majoritarian scholars and dissenting legislators who concede the underlying problem persists while disputing the judicial remedy. Attestation from minority communities is genuine but flows to a beneficiary seat and is weighted accordingly; the court's own attestation is self-interested and carries no independent probative weight.
narrative_ontology:disappearance_verdict(constitutional_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_interpretive_authority__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_interpretive_authority__judicial_supremacy_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. The claimed type is tangled_rope because the structure holds BOTH halves: a genuine coordination function (single terminating authority for fundamental-law disputes; rights enforcement against transient majorities) AND asymmetric extraction through the same structure (the court's interpretive authority compounds with each nullification; the legislature's output is revocable; the electorate's policy wins are overridable). Extractiveness 0.56 reflects that balance at interval end — real service, real yield. Suppression 0.60 is a raw structural property, NOT scaled by power or scope: finality forecloses rival interpretive authorities within the polity, judgments are executed by the executive, and defiance carries constitutional-crisis costs, while amendment and appointment channels remain open, keeping suppression below snare levels. Theater_ratio 0.30: nullification does real doctrinal work, but a growing share of activity is ceremonial guardianship rhetoric and symbolic opinion-writing that maintains the guardian self-image. Accessibility_collapse 0.50: within an adopting polity, legislative self-interpretation collapses as a legitimate practice, but rival settlements remain fully operational in other polities and coordinate practices survive at the margins (departmentalist protest, amendment politics). Resistance 0.62: recurring court-curbing bills, jurisdiction-stripping proposals, open-defiance episodes, and a sustained counter-majoritarian scholarly literature. The measurement series run on ONE shared time grid (points 0, 40, 80, 120, 160, 200, 220) with every tracked metric authored at every point; the trajectory is a consolidation arc — weak early enforcement, maturation, post-war rights-expansion peak, then a late plateau capped by legitimacy backlash rather than cyclical oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the court's seat the arrangement is fiduciary duty: it experiences nullification as obligation, and its identity_locked exit encodes institutional identity fusion — the organization has become its guardianship function, so the classification from that seat would shift only if the guardian frame itself broke. From the legislature's seat the identical structure is an ex post veto over enacted law held by an unaccountable branch. The electorate sits genuinely split: the same nullification that protects it also disenfranchises its policy victories, so its computed position depends on how its secondary beneficiary role weighs against its payer role. Minority communities experience the arrangement almost purely as subsidy; the professoriate as an ecosystem that feeds it. The engine derives these divergent per-seat classifications from the power, exit, and role data — the divergence is the finding, not something the author adjudicates.
 *
 * DIRECTIONALITY LOGIC:
 *   The apex court is declared beneficiary (and agenda_setter), so its derived directionality sits near the beneficiary end despite running the arrangement — its yield IS the arrangement's output. The national legislature and subnational legislatures are declared victims (payer role) with constrained exits, placing them near the full-target end; their counter-moves are real but supermajoritarian and judicially glossed, so they modulate rather than escape. The democratic electorate is payer with secondary beneficiary: derived directionality lands mid-to-high, tempered by rights gains it could not secure legislatively. Minority rights communities are beneficiaries with trapped exit — subsidized, near the beneficiary end, and their trap cuts both ways since the same unaccountable protector can withdraw protection. The professoriate is a mobile beneficiary collecting prestige and career rents incidentally. National spatial scope modestly amplifies effective extraction for target seats (harder verification of compliance at scale); suppression enters the computation unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — majoritarian rights violation and the absence of a terminating interpretive authority — is still live, so this is not a mandatrophy case: no sunset clause exists or should, and the arrangement persists because the problem persists AND because fixing is prohibitively expensive (entrenched amendment thresholds, judicially defined limits on amendment itself). The tangled_rope classification prevents two opposite mislabels: a pure-rope reading would erase the legislature's subordination and the electorate's lost policy control (the extraction half); a pure-snare reading would erase the rights protection that powerless communities demonstrably receive and could not otherwise obtain (the coordination half). Theater_ratio is tracked separately precisely so performative guardianship rhetoric is not mistaken for either function or extraction. The late-interval plateau in extractiveness alongside rising theater is the signature worth watching: if the coordination half continues to atrophy while nullification persists mainly as boundary maintenance, the drift path runs toward piton, not snare — but the current data do not support that call.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the judicial_supremacy_reading of the kernel constitutional_interpretive_authority; how would the classification change under the sibling readings?',
    'Re-classify under each sibling''s instantiation: the parliamentary_supremacy_reading removes the judiciary from the beneficiary set and dissolves the nullification victim class entirely; the coordinate_construction_reading distributes finality across branches so that no single seat captures interpretive authority and the extraction profile flattens toward coordination overhead.',
    'Epsilon, the beneficiary/victim structure, and the computed type are reading-indexed; the classification computed for this file licenses no inference about the sibling constraints, which must be authored and evaluated separately.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: one reading of a contested kernel, with reading-relative classification.').

omega_variable(
    locus_of_finality_disagreement,
    'Where exactly do the readings disagree — is interpretive finality a singularity that must lodge in some branch, a distributable property of inter-branch practice, or an illusion that no branch ever truly possesses?',
    'Conceptual analysis of constitutional theory combined with observation of how polities actually terminate interpretive disputes: apex-court last word, parliamentary override, or continuing inter-branch negotiation without termination.',
    'If finality is necessarily singular, the coordinate_construction_reading is unstable and the live contest reduces to judicial versus parliamentary supremacy; if finality is distributable, this reading''s core premise weakens and the arrangement trends toward ordinary multi-party coordination with correspondingly lower measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(locus_of_finality_disagreement, conceptual, 'The specific structural element on which the three sibling readings diverge: the locus of interpretive finality.').

omega_variable(
    guardianship_vs_rent_capture,
    'Is the court''s exercise of final authority predominantly fiduciary rights-enforcement, or self-interested interpretive rent-seeking that the guardianship language covers?',
    'Systematic coding of nullification decisions against ideological alignment between court majorities and governing coalitions, deference rates to allied governments, and the rate at which precedent reverses when court composition changes.',
    'Predominant capture pushes the effective classification toward snare — the coordination story functioning as cover for extraction; predominant fiduciary enforcement supports the tangled_rope reading with the court''s yield treated as compensation for a real enforcement service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guardianship_vs_rent_capture, empirical, 'Whether the judiciary''s yield reflects service rendered or rent collected.').

omega_variable(
    electorate_net_position,
    'Does the democratic electorate''s rights-protection gain outweigh its democratic-displacement loss — is the electorate seat net beneficiary or net payer?',
    'Preference-dependent: survey and deliberative evidence on whether voters would trade the judicial veto for fuller legislative control, disaggregated by whether their own preferred policies survived review.',
    'If net beneficiary, the electorate''s directionality drops toward symmetry and extraction concentrates harder on the legislature alone; if net payer, the victim class widens and aggregate effective extraction rises, pushing the arrangement toward the snare boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(electorate_net_position, preference, 'Value-dependent net position of the electorate under rights-guardianship versus democratic-control priorities.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__judicial_supremacy_reading, 0, 220).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(judicial_supremacy_reading_tr_t0, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t0, observed).
narrative_ontology:measurement(judicial_supremacy_reading_tr_t40, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t40, observed).
narrative_ontology:measurement(judicial_supremacy_reading_tr_t80, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t80, observed).
narrative_ontology:measurement(judicial_supremacy_reading_tr_t120, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 120, 0.25).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t120, observed).
narrative_ontology:measurement(judicial_supremacy_reading_tr_t160, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 160, 0.28).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t160, observed).
narrative_ontology:measurement(judicial_supremacy_reading_tr_t200, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 200, 0.3).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t200, observed).
narrative_ontology:measurement(judicial_supremacy_reading_tr_t220, constitutional_interpretive_authority__judicial_supremacy_reading, theater_ratio, 220, 0.3).
narrative_ontology:measurement_basis(judicial_supremacy_reading_tr_t220, observed).

% Extraction over time
narrative_ontology:measurement(judicial_supremacy_reading_be_t0, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t0, observed).
narrative_ontology:measurement(judicial_supremacy_reading_be_t40, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t40, observed).
narrative_ontology:measurement(judicial_supremacy_reading_be_t80, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 80, 0.46).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t80, observed).
narrative_ontology:measurement(judicial_supremacy_reading_be_t120, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 120, 0.53).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t120, observed).
narrative_ontology:measurement(judicial_supremacy_reading_be_t160, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 160, 0.57).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t160, observed).
narrative_ontology:measurement(judicial_supremacy_reading_be_t200, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 200, 0.57).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t200, observed).
narrative_ontology:measurement(judicial_supremacy_reading_be_t220, constitutional_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 220, 0.56).
narrative_ontology:measurement_basis(judicial_supremacy_reading_be_t220, observed).

% Suppression requirement over time
narrative_ontology:measurement(judicial_supremacy_reading_su_t0, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t0, observed).
narrative_ontology:measurement(judicial_supremacy_reading_su_t40, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t40, observed).
narrative_ontology:measurement(judicial_supremacy_reading_su_t80, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 80, 0.49).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t80, observed).
narrative_ontology:measurement(judicial_supremacy_reading_su_t120, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 120, 0.55).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t120, observed).
narrative_ontology:measurement(judicial_supremacy_reading_su_t160, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 160, 0.59).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t160, observed).
narrative_ontology:measurement(judicial_supremacy_reading_su_t200, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 200, 0.61).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t200, observed).
narrative_ontology:measurement(judicial_supremacy_reading_su_t220, constitutional_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 220, 0.6).
narrative_ontology:measurement_basis(judicial_supremacy_reading_su_t220, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__judicial_supremacy_reading, coordinate_construction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the kernel constitutional_interpretive_authority. The colloquial label 'who interprets the constitution' conflates three structurally distinct arrangements: judicial finality with nullification power (this file), parliamentary finality with no judicial voiding power (parliamentary_supremacy_reading), and distributed inter-branch construction with no final authority (coordinate_construction_reading). Each is a separate constraint with its own epsilon, beneficiary/victim structure, and classification; this reading puts the judiciary in the beneficiary set and the legislature in the victim set, a structural delta the siblings invert or dissolve. The stories are linked through network.affects_constraints so contamination and legitimacy-spillover analysis can traverse the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
