% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_restriction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_restriction_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_restriction_reading
 *   human_readable: GPL Reciprocity Obligation as Business-Model Restriction (Copyleft-as-Restriction Reading)
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   This story instantiates the copyleft_as_restriction_reading of the
 *   gpl_reciprocity_obligation kernel: the GNU General Public License's
 *   reciprocity condition (distribute a derivative work and you must publish
 *   its source) as experienced from the proprietary-business seat, where it
 *   operates as a prohibition on folding GPL-covered code into closed
 *   products. The epsilon referent is the standing arrangement under contest,
 *   the GPL reciprocity obligation itself, assessed by this reading's own
 *   lights: the reading counts surrendered product control, mandatory
 *   disclosure, and compliance overhead as costs borne by commercial
 *   integrators. The sibling readings, copyleft_as_freedom_reading and
 *   copyleft_as_commons_reading, are separate constraint files with their own
 *   epsilon, beneficiary structures, and classifications; nothing about them
 *   is averaged into this one. The claim/metrics posture is deliberate: the
 *   claimed_type states what this reading holds to be structurally true,
 *   while the metrics describe the arrangement's operation as this seat
 *   assesses it; the engine computes per-seat classifications from the
 *   structural data and owns any divergence.
 *
 * KEY AGENTS:
 *   - - proprietary_software_vendors: Primary target (powerful/constrained) — bears source-disclosure obligations and integration prohibitions on GPL-derived products
 *   - - embedded_device_manufacturers: Secondary target (moderate/trapped) — post-lock-in compliance demands on shipped firmware
 *   - - free_software_commons: Primary beneficiary (organized/identity_locked) — receives published improvements, retains the closure veto
 *   - - copyleft_enforcement_organizations: Agenda setter (organized/identity_locked) — administers license texts and pursues compliance
 *   - - corporate_dual_track_contributors: Dual-positioned payer/beneficiary (powerful/constrained) — pays compliance costs while collecting shared-infrastructure value
 *   - - would_be_proprietary_forkers: Excluded actor (powerful/constrained) — barred from the integration the reciprocity term prohibits
 *   - - permissive_license_projects: Excluded competitor ecosystem (organized/mobile)
 *   - - copyleft_downstream_users: Diffuse beneficiary (powerless/mobile)
 *   - - licensing_adjudicators: Analytical observer (institutional/analytical) — rulings constitute the kernel's operative legal meaning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.7).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.65).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, snare).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "GPL Reciprocity Obligation as Business-Model Restriction (Copyleft-as-Restriction Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_restriction_reading, "economic/legal/technological").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_restriction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '6508f514-0854-477a-aa0e-516784932977').
narrative_ontology:cs_kernel_codification('6508f514-0854-477a-aa0e-516784932977', fixed_text).
narrative_ontology:cs_authority_grounding('6508f514-0854-477a-aa0e-516784932977', distributed).
narrative_ontology:cs_reading_relation('6508f514-0854-477a-aa0e-516784932977', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('6508f514-0854-477a-aa0e-516784932977', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('6508f514-0854-477a-aa0e-516784932977', foundational, proprietary_integration_is_legitimate_business_model).
narrative_ontology:cs_axiom_status(proprietary_integration_is_legitimate_business_model, holdable).
narrative_ontology:cs_axiom_grounding('6508f514-0854-477a-aa0e-516784932977', proprietary_integration_is_legitimate_business_model, deontological).
narrative_ontology:cs_axiom('6508f514-0854-477a-aa0e-516784932977', secondary, reciprocity_exceeds_coordination_necessity).
narrative_ontology:cs_axiom_status(reciprocity_exceeds_coordination_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6508f514-0854-477a-aa0e-516784932977', reciprocity_exceeds_coordination_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('6508f514-0854-477a-aa0e-516784932977', proprietary_integration_liberty_norm).
narrative_ontology:cs_drift_state('6508f514-0854-477a-aa0e-516784932977', contemporary_cloud_saas_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6508f514-0854-477a-aa0e-516784932977', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_restriction_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_commons).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_device_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_dual_track_contributors).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_dual_track_contributors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The aggregate body of individuals and projects publishing code under GPL terms. Anyone may run, study, modify, and redistribute the code; whoever distributes derivative works must publish the corresponding source. Improvements submitted by commercial and volunteer developers alike accumulate in the shared corpus. The corpus cannot be relicensed without tracking down every contributing copyright holder, and the community's self-understanding is bound up with keeping the reciprocity term in place.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_commons, beneficiary,
    organized, generational, identity_locked, global).

% Nonprofit stewardship bodies in the mold of the Free Software Foundation and Software Freedom Conservancy that publish license texts and interpretive FAQs, advise projects, and pursue compliance when derivative works ship without source. Their budgets and staff are small relative to the firms they engage; settlements typically fund further compliance work. Their organizational purpose is constituted by administering and defending the license terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_enforcement_organizations, agenda_setter,
    organized, generational, identity_locked, global).

% Commercial software firms that build products on top of GPL-covered infrastructure, most consequentially the Linux kernel. Distributing a product containing GPL code obliges them to offer complete corresponding source to recipients, which conflicts with business models premised on closed source. Their options are to disclose, to architect products to keep GPL code at arm's length, to buy or negotiate alternative licenses where offered, or to re-platform onto permissively licensed equivalents, each carrying substantial engineering and strategic cost.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, proprietary_software_vendors, payer,
    powerful, biographical, constrained, global).

% Hardware makers shipping routers, set-top boxes, and IoT devices whose firmware incorporates GPL components. Compliance demands tend to arrive after products are locked into supply chains and certification cycles, when swapping the software stack is prohibitively expensive. Several have been respondents in well-known compliance actions; the practical response is usually settling and publishing source after the fact.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, embedded_device_manufacturers, payer,
    moderate, immediate, trapped, global).

% Large technology firms that employ engineers contributing to GPL projects while shipping proprietary products adjacent to them. They receive enormous value from the shared infrastructure, often far exceeding their compliance costs, while campaigning internally and publicly to hold the license boundary at lines favorable to their product architecture, such as kernel-versus-userspace and linking-versus-distribution distinctions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_dual_track_contributors, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gpl_reciprocity_obligation__copyleft_as_restriction_reading, corporate_dual_track_contributors, beneficiary).

% Firms and investors who would fold GPL-covered innovations directly into closed platforms if the license permitted it. The reciprocity term is precisely what bars the integration they want; they engage the license only through policy advocacy, acquisition of permissively licensed alternatives, or funding of compatibility efforts. Accepting the terms is the price of admission to the code, and they decline it.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, would_be_proprietary_forkers, excluded,
    powerful, biographical, constrained, global).

% Ecosystems publishing under BSD-, MIT-, and Apache-style terms, which permit proprietary integration without reciprocal disclosure. They compete with GPL projects for the same contributors and corporate users, and they absorb firms migrating away from copyleft. They articulate the integration-friendly counterposition in standards bodies and conferences rather than inside GPL project governance.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, permissive_license_projects, excluded,
    organized, generational, mobile, global).

% Individuals and organizations running GPL-licensed software. The license text grants them guarantees, including source availability and modification and redistribution rights, that survive upstream business decisions. Individually they have negligible influence over license governance; collectively they are the constituency the grant of freedoms runs to.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_downstream_users, beneficiary,
    powerless, generational, mobile, global).

% Courts in Germany, the United States, and elsewhere that have heard disputes over GPL validity, breach, and remedies. Their rulings determine the license's operative legal meaning, including whether the reciprocity condition is a permissible copyright condition or an unenforceable restraint. They hold no stake in outcomes and act only when parties bring disputes.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_restriction_reading, licensing_adjudicators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_restriction_reading, free_software_commons).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_restriction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a permanently open shared codebase: anyone may use, modify, and redistribute GPL-covered software, and anyone who distributes derivative works must publish corresponding source, so improvements aggregate in the commons rather than leaking into closed products.
% TRANSFER_FUNCTION: Moves source-disclosure obligations and derivative-work control from commercial integrators to the commons: integrators surrender proprietary control over anything built on GPL code; the commons receives published improvements and retains the legal basis to block closure.
% ABSENT_VOICES: Would-be proprietary forkers and embedded manufacturers facing compliance demands speak only through litigation defense, standards-body lobbying, and migration to permissive ecosystems; end users, the constituency the license text nominally addresses, are absent from license-governance debates; permissive-license advocates argue in adjacent forums rather than inside GPL governance.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, the major GPL codebases (the Linux kernel, GCC, coreutils) would be folded into proprietary products within product cycles; the commons' aggregation mechanism collapses; support-and-services business models built on copyleft guarantees would need new foundations; permissively licensed projects would absorb displaced contribution.
% FOUNDING_PROBLEM: Early free software was repeatedly taken proprietary: companies shipped improved versions of Emacs, Kerberos, and compiler tools with the improvements withheld, threatening the survival of the shared codebase. Reciprocal licensing was designed so that freedom, once granted, could not be revoked by commercial capture.
% FOUNDING_PROBLEM_CORROBORATION: Judicial opinions in GPL enforcement actions (German regional courts, Jacobsen v. Katzer in the United States) attest the license's operative purpose and history independently of the Free Software Foundation; academic intellectual-property scholarship documents the enclosure incidents that motivated the design; corporate contributors' public statements attest both the continuing pull of proprietary capture and their dispute over whether the remedy's costs now exceed its necessity. No attester outside the benefiting parties settles the status, hence contested.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_restriction_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_restriction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_restriction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_restriction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.70 because, from this seat, the license transfers substantial value (complete corresponding source for derivative works, surrendered control over product code, a compliance apparatus) while leaving alternatives partly open (permissive stacks, clean-room rewrites, negotiated licenses), which bounds rather than eliminates the transfer. Suppression (0.65) is authored as a raw structural property, unscaled by power or scope: the prohibition on proprietary integration is the arrangement's operating mode, carried by copyright law and selective enforcement, with a large anticipatory component (integration forgone under litigation risk before any demand arrives). Theater ratio (0.30) reflects enforcement that is mostly functional (actual source publication follows most demands) overlaid with growing compliance ritual (license scanning, audit documentation) inside large firms. Accessibility collapse (0.50): understanding the license does not close the option set; avoiding GPL code entirely remains available at cost. Resistance (0.65): the payer seats resist continuously through architecture, lobbying, ecosystem migration, and occasional litigation; open coalitions against the license are rare because public opposition carries reputational cost in developer communities, so the victims here, powerful and moderate actors, organize resistance through avoidance rather than confrontation. The temporal series share one seven-point grid (1989-2026): extractiveness rises with the spread of GPL infrastructure into commerce and plateaus as permissive ecosystems absorb marginal commercial demand; suppression_requirement traces the enforcement ratchet (violation-letter campaigns, dedicated enforcement organizations, funded litigation) cresting around 2014-2020 and easing slightly amid post-Vizio curability uncertainty.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute differently. From the enforcement organizations' position the arrangement is a functioning guarantee mechanism they administer; from the commons' position it is the constitutive rule of a shared corpus; from the embedded manufacturers' position it is a post-lock-in compliance trap; from the dual-track corporations' position it is a bargain they pay into and draw from simultaneously. Same-power actors diverge on exit: powerful vendors are constrained (re-platforming is possible at cost), embedded manufacturers are trapped (mid-lifecycle stack swaps are prohibitive), would-be forkers are constrained from outside the terms. The engine computes these per-seat classifications from the structural data; the authored snare claim is this reading's claim, not an adjudication among seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (free_software_commons, copyleft_downstream_users) place those seats near the subsidized end: the license runs guarantees and published improvements toward them. Victim declarations (proprietary_software_vendors, embedded_device_manufacturers) place those seats near the target end; trapped exit pushes embedded manufacturers nearer full-target than the constrained-but-mobile-at-cost vendors. Enforcement organizations carry no beneficiary declaration because they administer rather than collect, and their identity_locked exit reflects a mission constituted by the license's administration. Corporate dual-track contributors are declared payer with secondary beneficiary, capturing the bargain structure. No directionality overrides are authored: the role-plus-exit declarations already differentiate every seat the derivation needs to distinguish, and the coarse power-atom keying of overrides would blur distinctions the stakeholder surface draws finely. Effective extraction is the engine's computation from these declarations, power, exit, and scope; only extractiveness scales, suppression enters unscaled.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem, preventing commercial capture of a shared codebase, is authored contested: enclosure pressure persists (SaaS delivery skirts distribution-triggered reciprocity entirely), while the payer seats contend the remedy's costs now exceed its necessity. Because the founding problem is not plainly dead, no mandatrophy resolution is declared, and the disappearance verdict is world_rearranges: the corpus, the support-and-services business models, and the compliance industry all depend on the arrangement. The classification discipline bites in both directions here. The restriction reading's characteristic error is mislabeling live coordination as pure extraction; the snare claim is falsifiable against the coordination record, and the omega reciprocity_exceeds_coordination_necessity routes exactly that test (would permissive licensing sustain a comparable commons?). The inverse error, reading the arrangement as pure coordination and missing the asymmetric burden, is checked by the receipt surface: gains demonstrably accrue to the commons seat, and fixing (relicensing a dispersed corpus) is prohibitive for anyone, which keeps the asymmetry question live rather than letting coordination framing absorb it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the copyleft_as_restriction_reading of kernel gpl_reciprocity_obligation: if the sibling readings (copyleft_as_freedom_reading, copyleft_as_commons_reading) were instantiated instead, which structural elements change?',
    'Comparative classification across the three sibling stories: hold the referent fixed (the GPL reciprocity arrangement), vary the reading, and observe the beneficiary/victim sets and epsilon each reading authors.',
    'The freedom reading relocates beneficiaries to end users and authors lower epsilon; the commons reading recasts the arrangement as enclosure-preventing coordination with lower measured extraction. This reading''s high epsilon is seat-specific, not topic-invariant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed variance in beneficiary structure and epsilon across the GPL kernel''s three readings.').

omega_variable(
    adoption_delta_proprietary_forks,
    'If the restriction reading prevailed as the operative legal-political interpretation (copyleft treated as an illegitimate restraint on business models), would benefit shift to proprietary vendors, commons contributors become victims, and proprietary forks proliferate?',
    'Track adjudicated challenges to GPL enforceability, legislative and procurement treatment of copyleft, and observed forking behavior following enforcement setbacks.',
    'A realized shift would invert the current victim/beneficiary mapping downstream: weakened enforcement would enable proprietary forks, the commons would bear enclosure losses, and the standing arrangement''s effective extraction profile would move toward the vendor seats'' advantage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adoption_delta_proprietary_forks, empirical, 'Downstream structural consequences of the restriction reading winning the kernel contest.').

omega_variable(
    vendor_frame_neutrality_ambiguity,
    'Is the restriction reading a neutral analytical description of the GPL''s effects on business models, or a self-serving frame advanced by parties seeking to escape reciprocity obligations?',
    'Compare objectors'' stated positions against revealed behavior: firms that litigate or lobby against copyleft while continuing to consume GPL infrastructure reveal a preference for the subsidy without the obligation.',
    'If self-serving, this reading''s epsilon overstates extraction by counting foregone enclosure opportunities as harm borne; if neutral, the compliance burden stands as genuine extraction from commercial integrators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_frame_neutrality_ambiguity, conceptual, 'Whether the restriction frame is analytic or strategic.').

omega_variable(
    chilling_vs_enforced_suppression,
    'How much of the measured suppression is produced by active enforcement versus anticipatory chilling (integration decisions forgone due to litigation risk without any demand ever arriving)?',
    'Compare integration rates and compliance postures across jurisdictions and periods with different enforcement intensities; survey vendor counsel on decision drivers.',
    'If chilling dominates, suppression persists even as formal enforcement decays and the suppression_requirement series understates the arrangement''s hold; if enforcement dominates, decay in enforcement capacity predicts erosion of the prohibition''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_vs_enforced_suppression, empirical, 'Composition of measured suppression between active enforcement and anticipatory chilling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_restriction_reading, 1989, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_restriction_reading_tr_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1989, 0.1).
narrative_ontology:measurement(gpl_restriction_reading_tr_t1995, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(gpl_restriction_reading_tr_t2001, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2001, 0.18).
narrative_ontology:measurement(gpl_restriction_reading_tr_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2007, 0.22).
narrative_ontology:measurement(gpl_restriction_reading_tr_t2014, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(gpl_restriction_reading_tr_t2020, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement(gpl_restriction_reading_tr_t2026, gpl_reciprocity_obligation__copyleft_as_restriction_reading, theater_ratio, 2026, 0.3).

% Extraction over time
narrative_ontology:measurement(gpl_restriction_reading_be_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1989, 0.35).
narrative_ontology:measurement(gpl_restriction_reading_be_t1995, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(gpl_restriction_reading_be_t2001, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2001, 0.58).
narrative_ontology:measurement(gpl_restriction_reading_be_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2007, 0.66).
narrative_ontology:measurement(gpl_restriction_reading_be_t2014, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(gpl_restriction_reading_be_t2020, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2020, 0.71).
narrative_ontology:measurement(gpl_restriction_reading_be_t2026, gpl_reciprocity_obligation__copyleft_as_restriction_reading, base_extractiveness, 2026, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(gpl_restriction_reading_su_t1989, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1989, 0.15).
narrative_ontology:measurement(gpl_restriction_reading_su_t1995, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 1995, 0.25).
narrative_ontology:measurement(gpl_restriction_reading_su_t2001, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2001, 0.42).
narrative_ontology:measurement(gpl_restriction_reading_su_t2007, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2007, 0.52).
narrative_ontology:measurement(gpl_restriction_reading_su_t2014, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement(gpl_restriction_reading_su_t2020, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2020, 0.63).
narrative_ontology:measurement(gpl_restriction_reading_su_t2026, gpl_reciprocity_obligation__copyleft_as_restriction_reading, suppression_requirement, 2026, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_restriction_reading, resource_allocation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_restriction_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the GPL' decomposes into three structurally distinct readings of one kernel (gpl_reciprocity_obligation), per the epsilon-invariance principle. This file authors the restriction reading (epsilon 0.70, beneficiaries = commons seats, victims = commercial integrators, claimed snare). The freedom reading relocates beneficiaries to end users and authors lower epsilon; the commons reading recasts the arrangement as enclosure-preventing coordination technology. All three readings cite the same incidents (Emacs/Kerberos capture attempts, compliance actions, court rulings) as evidence for their own frames; the license text and its enforcement history are the common referent. Epsilon differs across members because epsilon is reading-indexed over a fixed referent, not topic-invariant. Family members link via affects_constraints; orphaning any member would break contamination-propagation analysis across the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
